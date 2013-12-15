(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (factorial-2 n)
  (define (fact-iter prod cnt)
    (if (> cnt n)
        prod
        (fact-iter (* prod cnt) (+ cnt 1))))
  (fact-iter 1 1))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))



(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) 
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-it n)
  (define (iter f1 f2 f3 cnt)
    (if (> cnt n)
        f1
        (iter (+ f1 (* 2 f2) (* 3 f3)) f1 f2 (+ cnt 1))))
  (if (< n 3)
      n
      (iter 2 1 0 3)))

(define (pascal n k)
  (cond ((or (= k 0) (= k n)) 1)
        ((or (< k 0) (> k n)) 0)
        (else (+ (pascal (- n 1) (- k 1)) 
                 (pascal (- n 1) k)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(define (fast-expt b n)
  (define (iter a c l)
    (cond ((<= l 0) a)
          ((even? l) (iter a (* c c) (/ l 2)))
          (else (iter (* a c) c (- l 1)))))
  (iter 1 b n))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (fast-mul a b)
  (cond ((<= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))

(define (fast-mul-it b n)
  (define (iter a c l)
    (cond ((<= l 0) a)
          ((even? l) (iter a (+ c c) (halve l)))
          (else (iter (+ a c) c (- l 1)))))
  (iter 0 b n))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p)
                      (square q))      ; compute p'
                   (+ (square q)
                      (* 2 p q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

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

; bad primality test
(define (prime? n)
  (= n (smallest-divisor n)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; search for primes in [a, b]
(define (search-for-primes a b)
  ; test the number n and iterate
  (define (test n)
    (cond ((<= n b)
           (timed-prime-test n)
           (test (+ n 2)))))
  (if (even? a)
      (test (+ a 1))
      (test a)))

(define (carmichael? n)
  (define (test a)
    (cond ((>= a n) #t)
          ((= (expmod a n n) a) 
           (test (+ a 1)))
          (else #f)))
  (test 1))

; base^exp mod m for miller rabin, returns 0 for non-trivial square root
(define (rabin-mod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and (not (= base 1))
                  (not (= base (- m 1)))
                  (= (remainder (square base) m) 1))
             0
             (remainder (square (expmod base (/ exp 2) m)) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

(define (rabin-test n)
  (define (try-it a)
    (not (= (rabin-mod a (- n 1) n) 1)))
  (try-it (+ 1 (random (- n 1)))))