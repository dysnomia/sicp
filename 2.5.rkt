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

; generic procedure for two arguments
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (not (eq? type1 type2))
;                    (let ((t1->t2 (get-coercion type1 type2))
;                          (t2->t1 (get-coercion type2 type1)))
;                      (cond (t1->t2
;                             (apply-generic op (t1->t2 a1) a2))
;                            (t2->t1
;                             (apply-generic op a1 (t2->t1 a2)))
;                            (else
;                             (error "No method for these types"
;                                    (list op type-tags)))))
;                    (error "No method for these types" (list op type-tags))))
;              (error "No method for these types"
;                     (list op type-tags)))))))

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
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((raise1 (raise a1 a2))
                          (raise2 (raise a2 a1)))
                      (cond (raise1
                             (apply-generic op raise1 a2))
                            (raise2
                             (apply-generic op a1 raise2))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types" (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

; generic arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) 
  (let ((type (type-tag x)))
    (let ((func (get 'equ? (list type type))))
      (func (contents x) (contents y)))))
(define (=zero? x) (apply-generic '=zero? x))

; machine numbers
(define (install-scheme-number-package)
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
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; rational numbers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (install-rational-package)
  ;; internal procedures  
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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
         (= (* (numer x) (denom y))
            (* (numer y) (denom x)))))
  (put '=zero? '(rational)
       (lambda (x)
         (= (numer x) 0)))
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
  (put-coercion 'rational         'scheme-number rational->scheme-number)
  (put-coercion 'real             'rational     real->rational)
  (put-coercion 'complex          'real  complex->real)
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

; install all the number packages
(define (install-numbers)
  (install-scheme-number-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-number-coercions)
  'done)
(install-numbers)


