(define (square x) (* x x))

; put and get implementation
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; coercion implementation
(define coercion-registry '())
(define (put-coercion t1 t2 fn) (set! coercion-registry (cons (list t1 t2 fn) coercion-registry)))
(define (get-coercion t1 t2)
  (define (rec entry . reg)
    (define t1-entry car)
    (define t2-entry cadr)
    (define fn-entry caddr)
    (cond ((and (eq? t1 (t1-entry entry))
                (eq? t2 (t2-entry entry))) (fn-entry entry))
          ((null? reg) false)
          (else (apply rec reg))))
  (apply rec coercion-registry))

; type tags
(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; list operations
(define (nth n l)
  (if (= n 1) 
      (car l)
      (nth (- n 1) (cdr l))))
(define (replace-nth n l new)
  (if (= n 1)
      (cons new (cdr l))
      (cons (car l)
            (replace-nth (- n 1) (cdr l) new))))
(define (make-interval a b)
  (if (> a b)
      '()
      (cons a (make-interval (+ a 1) b))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (fold-right op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 

; generic procedure for two arguments
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types" (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

; generic procedure for multiple arguments
;(define (apply-generic op . args)
;  ; conversion data type
;  (define (get-from conv) (car conv))
;  (define (get-to conv) (cadr conv))
;  (define (make-conv from to) (list from to))
;  
;  ; given n types compute the list of conversions to try
;  (define (build-convs n)
;    (flatmap (lambda (to)
;           (map (lambda (from)
;                 (make-conv from to))
;                (make-interval (+ to 1) n)))
;         (make-interval 1 (- n 1))))
;  
;  ; try all available conversions
;  (define (apply-iter args convs)
;    (let ((type-tags (map type-tag args)))
;       (let ((proc (get op type-tags)))
;         (if proc
;             ; procedure found -> apply
;             (apply proc (map contents args))
;             (if (not (null? convs))
;                 ; try next conversion
;                 (let ((from (get-from (car convs)))
;                       (to (get-to (car convs))))
;                   (let ((from-type (nth from type-tags))
;                         (to-type (nth to type-tags)))
;                    (let ((from->to (get-coercion from-type to-type)))
;                      ; conversion exists -> do it
;                      (if from->to
;                          (apply-iter (replace-nth from args (from->to (nth from args))) (cdr convs))
;                          (apply-iter args (cdr convs))))))
;                 (error "No method for these types" (list op type-tags)))))))
;  (apply-iter args (build-convs (length args))))
  
; generic operation with tower and two arguments
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (drop (apply proc (map contents args)))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (not (eq? type1 type2))
;                    (let ((raise1 (raise a1 a2))
;                          (raise2 (raise a2 a1)))
;                      (cond (raise1
;                             (apply-generic op raise1 a2))
;                            (raise2
;                             (apply-generic op a1 raise2))
;                            (else
;                             (error "No method for these types"
;                                    (list op type-tags)))))
;                    (error "No method for these types" (list op type-tags))))
;              (error "No method for these types"
;                     (list op type-tags)))))))

; generic arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (gcd x y) (apply-generic 'gcd x y))
(define (reduce x y) (apply-generic 'reduce x y))
;(define (equ? x y) 
;  (let ((type (type-tag x)))
;    (let ((func (get 'equ? (list type type))))
;      (func (contents x) (contents y)))))
(define (=zero? x) (apply-generic '=zero? x))

; machine numbers
(define (install-scheme-number-package)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'gcd '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (x y) 
         (let ((g (gcd x y)))
           (list (tag (/ x g)) (tag (/ y g))))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; rational numbers

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (install-rational-package)
  ;; internal procedures  
  ;(define (make-rat n d)
  ;  (let ((g (gcd n d)))
  ;    (cons (/ n g) (/ d g))))
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y)  
         (equ? (mul (numer x) (denom y))
            (mul (numer y) (denom x)))))
  (put '=zero? '(rational)
       (lambda (x)
         (equ? (numer x) 0)))
  (put 'neg '(rational)
       (lambda (x)
         (tag (make-rat (- (numer x)) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

; real numbers
(define (install-real-package)
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(real)
       (lambda (x) (tag (- x))))
  ; TODO: add floating point errors
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'make 'real (lambda (x) (tag x)))
  'done)
(define (make-real x)
  ((get 'make 'real) x))

; complex numbers
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (install-polar-package)
  (install-rectangular-package)
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) 
         (and (= (real-part z1) (real-part z2))
              (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z) 
         (and (= (real-part z) 0)
              (= (imag-part z) 0))))
  (put 'neg '(complex)
       (lambda (z) 
         (tag (make-from-real-imag (- (real-part z)) (- (imag-part z))))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; type coercion -> tower of types
(define (install-number-coercions)
  ; raise in tower
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (rational->real n)
    (let ((numer (numer (contents n)))
          (denom (denom (contents n))))
      (make-real (/ numer denom))))
  (define (real->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'rational      'real     rational->real)
  (put-coercion 'real          'complex  real->complex)
  
  ; drop in tower
  (define (rational->scheme-number n)
    (numer (contents n)))
  (define (real->rational n)
    (make-rational (round (contents n)) 1))
  (define (complex->real n)
    (make-real (real-part (contents n))))
  (define (scheme-number->polynomial n)
    (make-polynomial 'y (list (make-term 0 (contents n)))))
  (put-coercion 'rational         'scheme-number rational->scheme-number)
  (put-coercion 'real             'rational     real->rational)
  (put-coercion 'complex          'real  complex->real)
  (put-coercion 'scheme-number    'polynomial  scheme-number->polynomial)
  'done)
(define tower '(scheme-number rational real complex))
(define tower-rev (reverse tower))


; raise arg1 to type of arg2 or return false if this is impossible
(define (raise a1 a2)
  (let ((type1 (type-tag a1))
        (type2 (type-tag a2)))
    (let ((tower1 (memq type1 tower))
          (tower2 (memq type2 tower)))
      (cond ((= (length tower1) (length tower2)) a1)
            ((< (length tower1) (length tower2)) #f)
            (else (let ((coerce (get-coercion type1 (cadr tower1))))
                    (raise (coerce a1) a2)))))))


; drop argument as far as possible
(define (drop a)
  (let ((type (type-tag a)))
    (let ((drop-tower (memq type tower-rev)))
      (if (= (length drop-tower) 1) 
          a
          ; try to drop it one step
          (let ((project (get-coercion type (cadr drop-tower)))
                (raise   (get-coercion (cadr drop-tower) type)))
            (let ((projected (project a)))
              (let ((raised (raise projected)))
                (if (equ? raised a)
                    (drop projected)
                    a))))))))

; terms in a polynomial
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;<procedures adjoin-term ...coeff from text below>

  ;; continued on next page

  ; negation
  (define (neg-poly p)
    (make-poly (variable p) (neg-termlist (term-list p))))
  (define (neg-termlist l)
    (map (lambda (term)
           (let ((cur-order (order term))
                 (cur-coeff (coeff term)))
             (make-term cur-order (neg cur-coeff))))
         l))       
  
  ; addition
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  ; subtraction
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  
  ; multiplication
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  ; division
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-result (div-terms (term-list p1) (term-list p2))))
          (list (tag (make-poly (variable p1) (car div-result)))
                (tag (make-poly (variable p2) (cadr div-result)))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((div-term (make-term new-o new-c)))
                  ; actual polynomial division
                  (let ((new-l1 (term-list (contents (sub (make-polynomial 'x L1) (mul (make-polynomial 'x L2) (make-polynomial 'x (list div-term))))))))
                    (let ((rest-of-result (div-terms new-l1 L2)))
                      (list (cons div-term (car rest-of-result)) (cadr rest-of-result))))))))))
  
  ; gcd
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))
  (define (pseudo-remainder-terms a b)
    (let ((t1 (first-term a))
          (t2 (first-term b)))
      (let ((factor (expt (coeff t2) (- (+ 1 (order t1)) (order t2)))))
        (let ((new-a (map (lambda (term)
                            (make-term (order term) (mul factor (coeff term))))
                          a)))
          (cadr (div-terms new-a b))))))
  
  ; reduce terms by their gcd
  (define (reduce-poly p1 p2)
    (let ((reduced (reduce-terms (term-list p1) (term-list p2))))
      (list (tag (make-poly (variable p1) (car reduced)))
            (tag (make-poly (variable p2) (cadr reduced))))))
  (define (reduce-terms t1 t2)
    (let ((g (gcd-terms t1 t2)))
      (let ((f1 (first-term t1))
            (f2 (first-term t2))
            (fg (first-term g)))
        (let ((factor (expt (coeff fg) (- (+ 1 (max (order f1) (order f2))) (order fg)))))
          (let ((red1 (div-terms (mul-out t1 factor) g))
                (red2 (div-terms (mul-out t2 factor) g)))
            (list (reduce-integer t1)
                  (reduce-integer t2)))))))
  (define (div-out t f)
    (map (lambda (term) (make-term (order term) (div (coeff term) f))) t))
  (define (mul-out t f)
    (map (lambda (term) (make-term (order term) (mul (coeff term) f))) t))
  (define (reduce-integer t)
    (let ((g (fold-right (lambda (cur right) (gcd cur right)) 0 (map coeff t))))
      (div-out t g)))
               
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (reduce-integer a)
        (gcd-terms b (pseudo-remainder-terms a b))))
    
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (div-poly p1 p2)))
  (put 'gcd '(polynomial polynomial) 
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial) 
       (lambda (p1 p2) (reduce-poly p1 p2)))
  (put 'neg '(polynomial) 
       (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) 
       (lambda (p) (= (length (term-list p)) 0)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; install all the number packages
(define (install-numbers)
  (install-scheme-number-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-number-coercions)
  (install-polynomial-package)
  'done)
(install-numbers)

(define p1 (make-polynomial 'x '((2 1)(1 -2)(0 1))))
(define p2 (make-polynomial 'x '((2 11)(0 7))))
(define p3 (make-polynomial 'x '((1 13)(0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
