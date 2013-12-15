(define (square x) (* x x))

; bank balance
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  ; constructor and dispatcher with account-pwd
  (define (dispatch account-pwd)
    (let ((pwd-wrong 0))
      (lambda (input-pwd m)
        (if (eq? input-pwd account-pwd)
            (begin
              (set! pwd-wrong 0)
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    ((eq? m 'clone) clone)
                    (else (error "Unknown request -- MAKE-ACCOUNT"
                                 m))))
            (begin
              (set! pwd-wrong (+ pwd-wrong 1))
              (if (not (> pwd-wrong 7))
                  "Incorrect password"
                  "Call the cops"))))))
  (define (clone new-pwd)
    (dispatch new-pwd))
  (clone password))

; accumulator
(define (make-accumulator sum)
  (lambda (amt)
    (begin
      (set! sum (+ sum amt))
      sum)))

(define (make-monitored f)
  (let ((times 0))
    (lambda args
      (cond ((and (= (length args) 1) (eq? (car args) 'how-many-calls?)) times)
            ((and (= (length args) 1) (eq? (car args) 'reset-counter)) (set! times 0))
            (else (begin
                    (set! times (+ times 1))
                  (apply f args)))))))

; probabilistic calculation of pi
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; estimate pi by testing gcd(x, y) = 1
(define (rand)
  (random 100000))
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (random-real)
  (/ (random 10000) 10000.0))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))

; estimate an integral by choosing points inside
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (integral-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (* (monte-carlo trials integral-test)
     (- x2 x1)
     (- y2 y1)))
(define (circle-predicate cx cy cr)
  (lambda (x y)
    (<= (+ (square (- x cx))
           (square (- y cy)))
        (square cr))))
(define unit-circle? (circle-predicate 0.0 0.0 1.0))

; new (silly) random generator
(define rand
  (let ((x 0))
    (lambda (type)
      (cond ((eq? type 'generate)
             (set! x (+ x 1))
             x)
            ((eq? type 'reset)
             (lambda (init)
               (set! x init)))
            (else (error "Bad request"))))))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

; function with side effects
(define f
  (let ((local 0))
    (lambda (n)
      (let ((prev-local local))
        (set! local n)
        prev-local))))

; 
(define f
  (let ((count 0))
    (lambda ()
      (begin (set! count (+ count 1))
             count))))
  