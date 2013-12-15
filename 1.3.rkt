; Sum from a to b of term(i) with next as step
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Simpson integration
(define (simpson f a b n)
  ; area of trapez
  (define h
    (/ (- b a) n))
  (define (func k)
    (+ (f (+ a (* (- k 1) h))) 
       (* 4 (f (+ a (* k h))))
       (f (+ a (* (+ k 1) h)))))
  (define (inc k)
    (+ k 2))
  (/ (* h (sum func 1 inc n)) 3))

; iterative version of the sum
(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; general product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

; product: iterative
(define (product-it term a next b)
  (define (iter a prod)
    (if (> a b)
        prod
        (iter (next a) (* prod (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

; Wallis product for pi
(define (wallis n)
  (define (p k)
    (/ (square (* 2 k))
       (square (- (* 2 k) 1))))
  (define (inc k) (+ k 1))
  (/ (* 8 (product p 2. inc n)) (* 2 n)))

; accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; accumulate: iterative
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; accumulate: filter version
(define (accumulate-filter combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

; test progs
(define (next n)
  (if (= n 2) 
      3
      (+ n 2)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (and (= n (smallest-divisor n))
       (not (= n 1))))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; filters
(define (sum-primes a b)
  (accumulate-filter + 0 square a inc b prime?))
(define (phi n)
  (define (rel-prime a)
    (= (gcd a n) 1))
  (accumulate-filter * 1 identity 1 inc (- n 1) rel-prime))

; nonsense
(define (f g)
  (g 2))

; half-interval method
(define (average x y)
  (/ (+ x y) 2))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

; fixed point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (fixed-point-avg f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (average guess (f guess))))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; golden ratio
;(define golden
;  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.))

; solution to x^x = 1000
;(define sol
;  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
;(define sol-avg
;  (fixed-point-avg (lambda (x) (/ (log 1000) (log x))) 2.0))

; continued fraction
(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    result)))))
  (iter k 0))

; continued fraction: recursive
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (>= i k)
        0
        (/ (n i)
           (+ (d i)
              (rec (+ i 1))))))
  (rec 1))

; continued fraction for exp
(define (euler n)
  (define (d n)
    (if (= (remainder n 3) 2)
        (* 2 (/ (+ n 1) 3))
        1))
  (+ (cont-frac (lambda (x) 1.) d n) 2))

; continued fraction for tangent
(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

; newtons method
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; cubic: x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))
(define (average-damp f)
  (lambda (x) (average x (f x))))

; double function
(define (double f)
  (lambda (x) (f (f x))))

; compose functions
(define (compose f g)
  (lambda (x) (f (g x))))

; repeated functions
(define (repeated f n)
  (if (<= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

; smoothed function
(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

; n-th root
(define (root n x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (expt y (- n 1))))
   (repeated average-damp (/ n 4))
   1.0))

; iterative improvement
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter guess)))
(define (sqrt-2 x)
  ((iterative-improve 
   (lambda (y) (< (abs (- (square y) x)) 0.0001))
   (lambda (y) (average y (/ x y)))) 1.0))