(define (sumSqr x y) 
  (+ (* x x)
     (* y y)))

(define (maxSqr a b c)
  (cond ((and (> a c) (> b c)) (sumSqr a b))
        ((and (> a b) (> c b)) (sumSqr a c))
        (else (sumSqr b c))))

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(define (square x)
  (* x x))

(define (sqrt-iter guess guess-old x)
  (if (good-enough? guess guess-old)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess guess-old)
  (< (/ (abs (- guess guess-old)) guess) 0.001))

(define (mysqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube-iter guess guess-old x)
  (if (good-enough? guess guess-old)
      guess
      (cube-iter (improve-cube guess x)
                 guess
                 x)))

(define (mycube x)
  (cube-iter 1.0 0.0 x))

(define (f n)
  (define (f-iter k f3 f2 f1)
    (if (> k n)
        f1
        (f-iter (+ k 1)
                f2
                f1
                (+ f1 (* 2 f2) (* 3 f3)))))
  (if (< n 3)
      n
      (f-iter 3 0 1 2)))

(define (f-slow n)
  (if (< n 3)
      n
      (+ (f-slow (- n 1))
         (* 2 (f-slow (- n 2)))
         (* 3 (f-slow (- n 3))))))
