; standard ops 
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; operations on streams
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (x) (stream-car x)) argstreams))
       (apply stream-map
              (cons proc (map (lambda (x) (stream-cdr x)) argstreams))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s n)
  (if (> n 0)
      (begin
        (display-line (stream-car s))
        (display-stream (stream-cdr s) (- n 1)))))

(define (display-line x)
  (newline)
  (display x))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; stream primes
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define s1 (stream-enumerate-interval 2 1000))
(define s2 (stream-enumerate-interval 1001 4000))

;(define (show x)
;  (display-line x)
;  x)
;(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)
;
;(define sum 0)
;(define (accum x)
;  (set! sum (+ x sum))
;  sum)
;(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;(define y (stream-filter even? seq))
;(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                         seq))
;(stream-ref y 7)
;(display-stream z)

; integers
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
;(define ones (cons-stream 1 ones))
;(define integers (cons-stream 1 (add-streams ones integers)))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
;(define s (cons-stream 1 (add-streams s s)))
(define ones (cons-stream 1 ones))
(define factorials (cons-stream 1 (mul-streams integers factorials)))

; partial sums of stream
(define (partial-sums s)
  (let ((front (stream-car s)))
    (cons-stream
     front
     (stream-map (lambda (x) (+ x front)) (partial-sums (stream-cdr s))))))

; hamming problem
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (integrate-series s)
  (mul-streams
   (stream-map (lambda (x) (/ 1 x)) integers)
   s))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (neg-stream s)
  (stream-map (lambda (x) (- x)) s))

(define (mul-series s1 s2)
  (let ((car1 (stream-car s1))
        (car2 (stream-car s2)))
    (cons-stream
     (* car1 car2)
     (add-streams
      (mul-streams s1 (stream-cdr s2))
      (mul-streams (stream-cdr s1) s2)))))

(define (invert-unit-series s)
  (cons-stream
   1
   (mul-series
    (neg-stream (stream-cdr s))
    (invert-unit-series s))))

(define (div-series s1 s2)
  (let ((denon (stream-car s2)))
    (if (= 0 denon)
        (error "Bad divisor")
        (let ((normalized (scale-stream s2 (/ 1 denon))))
          (mul-series
           s1
           (scale-stream (invert-unit-series normalized) denon))))))

; sqrt
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; approximation of pi
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; compute until tolerance
(define (stream-limit s tolerance)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s2)) tolerance)
        s2
        (stream-limit (stream-cdr s) tolerance))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; ln(2) using series
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))
(define ln-series
  (partial-sums (ln-summands 1)))

; pairs
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
       (stream-map (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
       (pairs (stream-cdr s) (stream-cdr t)))))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (triples s t u)
  (cons-stream 
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s)  x))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
(define int-pairs (pairs integers integers))
(define int-triples (triples integers integers integers))

(define pythagorean
  (stream-filter
   (lambda (x)
     (let ((i (car x))
           (j (cadr x))
           (k (caddr x)))
       (= (+ (square i) (square j))
          (square k))))
   int-triples))

; weighted pairs
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1w (weight s1car))
                 (s2w (weight s2car))) 
             (cond ((< s1w s2w)
                    (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                   (else
                    (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
       (stream-map (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
       (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
       weight)))

(define (ramanujan-weight x)
    (+ (cube (car x)) (cube (cadr x))))
(define ramanujan-base
  (weighted-pairs integers integers ramanujan-weight))
(define (ramanujan-iter s)
  (let ((w1 (ramanujan-weight (stream-car s)))
        (w2 (ramanujan-weight (stream-car (stream-cdr s)))))
    (if (= w1 w2)
        (cons-stream
         w1
         (ramanujan-iter (stream-cdr s)))
        (ramanujan-iter (stream-cdr s)))))
(define ramanujan-sequence
  (ramanujan-iter ramanujan-base))
  
; signal processing
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; RC circuit

; zero crossings
;(define zero-crossings
;  (stream-map sign-change-detector sense-data
;              (cons-stream 0 sense-data)))

; solving differential equations
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
; solve 1-st order
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
; solve 2-nd order
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)