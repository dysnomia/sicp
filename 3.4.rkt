(define (parallel-execute . thunks)
  (for-each thread thunks))

(define (make-mutex)
  (let ((cell (mcons false '())))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (test-and-set! cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))
(define (clear! cell)
  (set-mcar! cell false))
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define x 10)
(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))

; semaphore
(define (make-sema n)
  (let ((mutex (make-mutex)))
    (define (acquire)
      (begin
        (mutex 'acquire)
        (if (> n 0)
            (begin
              (set! n (- n 1))
              (mutex 'release)
              #t)
            (begin
              (mutex 'release)
              (acquire)))))
    (define (release)
      (begin
        (mutex 'acquire)
        (set! n (+ n 1))
        (mutex 'release)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "No such method" m))))
    the-semaphore))

(define sema (make-sema 2))
