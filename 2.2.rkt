;(define (last-pair l)  
;  (if (null? (cdr l))
;      l
;      (last-pair (cdr l))))

; reverse list
;(define (reverse l)
;  (define (iter in out)
;    (if (null? in)
;        out
;        (iter (cdr in) (cons (car in) out))))
;  (iter l (list)))
      
; change
(define (count-change amount coin-values)
  (cc amount coin-values))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (no-more? coin-values) (null? coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))
(define us-coins (list 50 25 10 1 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; same parity...
(define (same-parity . l)
  (define (parity l num)
    (cond ((null? l) l)
          ((= num (remainder (car l) 2))
           (cons (car l) (parity (cdr l) num)))
          (else (parity (cdr l) num))))
  (cons (car l)
        (parity (cdr l) (remainder (car l) 2))))

; square list
(define (square x) (* x x))
;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (square (car items)) 
;            (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x))
       items))

; apply f to each element of list l
;(define (for-each f l)
;  (cond ((null? l) #t)
;        (else (f (car l))
;              (for-each f (cdr l)))))

; deep reverse
(define (deep-reverse l)
  (define (iter in out)
    (cond ((null? in) out)
          ((pair? (car in))
           (iter (cdr in) (cons (iter (car in) '()) out)))
          (else 
           (iter (cdr in) (cons (car in) out)))))
  (iter l '()))

; leaves of a tree
(define (fringe l)
  (cond ((null? l) '())
        ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

; binary mobile
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch m) (car m))
(define (right-branch m) (cdr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))
(define (total-weight mobile)
  (if (pair? mobile)
      (let ((l (left-branch mobile))
            (r (right-branch mobile)))
        (+ (total-weight (branch-structure l))
           (total-weight (branch-structure r))))
      mobile))

(define m-bal 
  (make-mobile 
   (make-branch 1 10)
   (make-branch 
    2
    (make-mobile
     (make-branch 1 4)
     (make-branch 4 1)))))
(define m-unbal
  (make-mobile
   (make-branch 2 3)
   (make-branch 3 3)))

; mobile balanced?
(define (balanced? mobile)
  (if (pair? mobile)
      (let ((l (left-branch mobile))
            (r (right-branch mobile)))
        (and (balanced? (branch-structure l))
             (balanced? (branch-structure r))
             (let ((ll (branch-length l))
                   (lr (branch-length r)))
               (= (* ll (total-weight (branch-structure l)))
                  (* lr (total-weight (branch-structure r)))))))
      #t))

;
(define ex-tree
  (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; map on whole tree
(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else
         ; here is the actual recursion
         (cons (tree-map f (car tree))
               (tree-map f (cdr tree))))))
(define (square-tree tree) (tree-map square tree))

; set of subsets
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; signal flows
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; common operations as accumulations
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;(define (append seq1 seq2)
;  (accumulate cons seq2 seq1))
;(define (length sequence)
;  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; polynomial as list: (a0, ..., an)
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff
                   (* higher-terms x)))
              0
              coefficient-sequence))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate +
              0 
              (map (lambda (tree)
                     (if (not (pair? tree))
                         1
                         (count-leaves tree)))
                   t)))

; accumulate multiple sequences
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init 
                (map (lambda (list) (car list)) seqs))
            (accumulate-n op init 
                (map (lambda (list) (cdr list)) seqs)))))

; vector and matrix operations
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define v '(-1 0 1 2))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

; rename accumulate to fold-right
(define (fold-right op initial sequence)
  (accumulate op initial sequence))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; reverse based on fold
;(define (reverse sequence)
;  (fold-right (lambda (x y) (append y (list x))) nil sequence))
;(define (reverse sequence)
;  (fold-left (lambda (x y) (cons y x)) nil sequence))

; map and flatten a sequence
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
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map 
      (lambda (j) (list i j))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
(define (unique-triples n)
  (flatmap
   (lambda (l)
     (let ((j (cadr l)))
       (map
        (lambda (k) (append l (list k)))
        (enumerate-interval 1 (- j 1)))))
   (unique-pairs n)))
; triples <= n that sum to s
;(define (filter predicate sequence)
;  (cond ((null? sequence) nil)
;        ((predicate (car sequence))
;         (cons (car sequence)
;               (filter predicate (cdr sequence))))
;        (else (filter predicate (cdr sequence)))))
(define (triple-sum n s)
  (filter 
   (lambda (l)
     (let ((i (car l))
           (j (cadr l))
           (k (caddr l)))
       (= (+ i j k) s)))
   (unique-triples n)))

; queens problem
(define empty-board '())
; new queen is at the front
(define (adjoin-position row col rest)
  (cons (list row col) rest))
; selectors
(define (row-pos pos) (car pos))
(define (col-pos pos) (cadr pos))
(define (new-pos positions) (car positions))
(define (old-pos positions) (cdr positions))

; does a queen at one position beat the queen at the other position
(define (beats? p1 p2)
  (let ((r1 (row-pos p1))
        (c1 (col-pos p1))
        (r2 (row-pos p2))
        (c2 (col-pos p2)))
    (or
     ; same row
     (= r1 r2)
     ; same col
     (= c1 c2)
     ; diagonal 1
     (= (+ r1 c1) (+ r2 c2))
     ; diagonal 2
     (= (- r1 c1) (- r2 c2)))))
           

; check whether setting the new queen has been set correctly?
(define (safe? col positions)
  ; check against all the old queens
  (let ((new (new-pos positions))
        (old (old-pos positions)))
    (fold-right
     (lambda (cur rest)
       (and (not (beats? new cur)) rest))
     #t
     old)))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; painting language
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; test-painter
(define rogers
  (load-painter "founder.gif"))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter
               (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (split f1 f2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f1 f2) painter (- n 1))))
          (f1 painter
                 (f2 smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

; frames and vectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect factor v)
  (make-vect (* factor (xcor-vect v))
             (* factor (ycor-vect v))))
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; line segment
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define outline-painter
  (segments->painter 
   (list (make-segment (make-vect 0 0.0) (make-vect 0.99 0.0))
         (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
         (make-segment (make-vect 0.99 0.99) (make-vect 0.0 0.99))
         (make-segment (make-vect 0.0 0.99) (make-vect 0.0 0.0)))))
(define x-painter
  (segments->painter 
   (list (make-segment (make-vect 0 0.0) (make-vect 0.99 0.99))
         (make-segment (make-vect 0 0.99) (make-vect 0.99 0)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 0.0 1.0))) ; new end of edge2