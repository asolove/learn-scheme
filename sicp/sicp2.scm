(module sicp2 scheme
  (require "sicp1.scm")
  
;;; Chapter 2: Building Abstractions with Data
; Rational numbers
; Ex 2.1
(define (rat-make n d)
  (if (negative? d)
      (cons (- n) (- d))
      (cons n d)))

(define rat-numer car)
(define rat-denom cdr)

(define (rat-add r1 r2)
  (rat-make (+ (* (rat-denom r1)
                  (rat-numer r2))
               (* (rat-denom r2)
                  (rat-numer r1)))
            (* (rat-denom r1)
               (rat-denom r2))))

(define (rat-sub r1 r2)
  (rat-add r1
           (rat-make (- (rat-numer r2))
                     (rat-denom r2))))

(define (rat-mult r1 r2)
  (rat-make (* (rat-numer r1)
               (rat-numer r2))
            (* (rat-denom r1)
               (rat-denom r2))))

(define (rat-div r1 r2)
  (rat-mult r1
            (rat-make (rat-denom r2)
                      (rat-numer r2))))

(define (rat-equal? r1 r2)
  (= (* (rat-denom r1)
        (rat-numer r2))
     (* (rat-numer r1)
        (rat-denom r2))))

(define (rat-print r)
  (print (rat-numer r))
  (print '/)
  (print (rat-denom r))
  (newline))

(define (rat-simplify r)
  (let* ((n (rat-numer r))
         (d (rat-denom r))
         (common (gcd n d)))
    (rat-make (/ n common)
              (/ d common))))

; Ex 2.2
(define (make-point x y)
  (cons x y))
(define point-x car)
(define point-y cdr)

(define (make-segment p1 p2)
  (cons p1 p2))
(define segment-p1 car)
(define segment-p2 cdr)


(define (segment-midpoint s)
  (make-point (average (point-x (segment-p1 s))
                       (point-x (segment-p2 s)))
              (average (point-y (segment-p1 s))
                       (point-y (segment-p2 s)))))

; Ex 2.3
; procedural layer
(define (rect-area r)
  (* (rect-base r)
     (rect-height r)))

(define (rect-perimeter r)
  (+ (* 2 (rect-base r))
     (* 2 (rect-height r))))

; data layer 1:
(define (make-rect p1 p2)
  (cons p1 p2))
(define rect-p1 car)
(define rect-p2 cdr)

(define (rect-base r)
  (- (point-x (rect-p2 r))
     (point-x (rect-p1 r))))

(define (rect-height r)
  (- (point-y (rect-p2 r))
     (point-y (rect-p1 r))))

; data layer 2:
;(define (make-rect point height base)
;  (list point height base))
;(define rect-point first)
;(define rect-height second)
;(define rect-base third)

; Ex 2.4
(define (cdr-funny cons)
  (cons (lambda (a b) b)))

; Ex 2.5
(define (exp-cons a b)
  (* (expt a 2)
     (expt b 3)))

(define (exp-car c)
  (if (divides? 2 c)
      (+ 1 (exp-car (/ c 2)))
      0))

(define (exp-cdr c)
  (if (divides? 3 c)
      (+ 1 (exp-cdr (/ c 3)))
      0))

; Ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; n = zero, f -> x -> f ((n f) x)
; f-> x-> f(x)
; (add-1 one) = f(f(x))
; Church numerals are just repeated function application
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-church n1 n2)
  (lambda (f) (lambda (x)
                ((n1 f) ((n2 f) x)))))

;; Intervals Ex 2.7
(define (make-interval low high)
  (cons low high))
(define interval-low car)
(define interval-high cdr)

(define (interval-add i1 i2)
  (make-interval (+ (interval-low i1)
                    (interval-low i2))
                 (+ (interval-high i1)
                    (interval-high i2))))

(define (interval-mult i1 i2)
  (let ((a (* (interval-low i1) (interval-low i2)))
        (b (* (interval-low i1) (interval-high i2)))
        (c (* (interval-high i1) (interval-low i2)))
        (d (* (interval-high i1) (interval-high i2))))
    (make-interval (min a b c d)
                   (max a b c d))))

(define (interval-div i1 i2)
  (if (and (> 0 (interval-low i2))
           (< 0 (interval-high i2)))
      (error "Division by interval containing 0")
      (interval-mult i1
                     (make-interval (/ 1 (interval-high i2))
                                    (/ 1 (interval-low i2))))))

; Ex 2.8
(define (interval-sub i1 i2)
  (interval-add i1
                (make-interval (- (interval-high i2))
                               (- (interval-low i2)))))

; Ex 2.9
(define (interval-width i)
  (/ (- (interval-high i) (interval-low i)) 2))

; width(i1 + i2) = width(i1) + width (i2)

; Ex 2.10
; see above

; Ex 2.11
(define interval-mult-simple interval-mult)

;(define (interval-mult i1 i2)
;  (let* ((low1 (interval-low i1))
;         (low2 (interval-low i2))
;         (high1 (interval-high i1))
;         (high2 (interval-high i2))
;         (interval-type (lambda (i)
;                          (cond ((> (interval-low i) 0) 'positive)
;                                ((< (interval-high i) 0) 'negative)
;                                (else 'mixed))))
;         (i1type (interval-type i1))
;         (i2type (interval-type i2)))
;
;    (cond ((and (eq? i1type 'positive)
;                (eq? i2type 'positive))
;           (make-interval (* low1 low2)
;                          (* high1 high2)))
;          ((and (eq? i1type 'negative)
;                (eq? i2type 'negative))
;           (make-interval (* high1 high2)
;                          (* low1 low2)))
;          ((and (eq? i1type 'positive)
;                (eq? i2type 'negative))
;           (make-interval (* high1 low2)
;                          (* low1 high2)))
;          ((and (eq? i1type 'negative)
;                (eq? i2type 'positive))
;           (make-interval (* high2 low1)
;                          (* high1 low2)))
;          ((and (eq? i1type 'positive)
;                (eq? i2type 'mixed))
;           (make-interval (* high1 low2)
;                          (* high1 high2)))
;          ((and (eq? i1type 'mixed)
;                (eq? i2type 'positive))
;           (make-interval (* high2 low1)
;                          (* high1 high2)))
;          ((and (eq? i1type 'negative)
;                (eq? i2type 'mixed))
;           (make-interval (* low1 high2)
;                          (* low1 low2)))
;          ((and (eq? i1type 'mixed)
;                (eq? i2type 'negative))
;           (make-interval (* low2 high1)
;                          (* low1 low2)))
;          (else
;           (interval-mult-simple i1 i2)))))

(define (make-center-percent c p)
  (let* ((width (* c p))
         (low (- c width))
         (high (+ c width)))
    (make-interval low high)))

(define (interval-center i)
  (/ (+ (interval-low i)
        (interval-high i))
     2))

(define (interval-percent i)
  (/ (interval-width i)
     (interval-center i)))

;(define (interval-width i)
;  (/ (- (interval-high i)
;        (interval-low i))
;     2))

; Ex 2.13
; tolerance of multiple roughly equals sum of
; tolerances of two factors

; Ex 2.14, 2.15, 2.16
; the simplest algebraic form that uses each interval term the least
; number of times leads to a tighter result. Algebraically, we can
; freely add terms (e.g a^2 / a is the same as a) that reduce the
; precision of the resulting interval. Solving this problem would seem
; to require a symbolic representation that could simplify computations
; and recognize terms needlessly reused.

;;; 2.2 Hierarchical Data and the Closure Property

; Ex 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

; Ex 2.18
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
              (list (car lst)))))

; Ex 2.19
(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else
         (+ (cc amount (cdr coins))
            (cc (- amount (car coins))
                coins)))))

(define uk-coins '(100 50 20 10 5 2 1 0.5))

; Ex 2.20
(define (same-parity . lst)
  (let* ((parity (odd? (car lst))))
    (filter (lambda (i)
              (eq? (odd? i) parity))
            lst)))

; Ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

;(define (square-list items)
;  (map (lambda (i) (* i i)) items))

; Ex 2.22
; This pattern of consing on answers while cdring down a list
; naturally results with the answers in reverse order, because
; we are putting each answer in front of the answers to questions
; that came before its question.
; Changing the order of args to cons won't work, because cons
; expects the second arg to already be a list, which it isn't.
; You need to use append, or else call reverse at the end.

; Ex 2.23
(define (for-each f l)
  (map f l)
  #t)

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((cons? tree) (+ (count-leaves (car tree))
                         (count-leaves (cdr tree))))
        (else 1)))

; Ex 2.27
(define (tree-reverse t)
  (cond ((null? t) '())
        ((list? t) (append (tree-reverse (cdr t))
                           (list (tree-reverse (car t)))))
        (else t)))

; Ex 2.28
(define (tree-fringe t)
  (cond ((null? t) '())
        ((list? t) (append (tree-fringe (car t))
                           (tree-fringe (cdr t))))
        (else (list t))))

; Ex 2.29
(define (make-mobile left right)
  (list left right))

(define left-branch first)
(define right-branch second)

(define (make-branch length structure)
  (list length structure))

(define branch-length first)
(define branch-structure second)

(define (total-weight mobile)
  (let ((left (branch-weight (left-branch mobile)))
        (right (branch-weight (right-branch mobile))))
    (+ left right)))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (cons? structure)
        (total-weight structure)
        structure)))

(define (mobile-balanced? m)
  (let ((left (left-branch m))
        (right (right-branch m)))
    (= (* (branch-length left)
          (branch-weight left))
       (* (branch-length right)
          (branch-weight right)))))

; d: just change the definitions from first and second to car and cdr.

; Ex 2.30, 2.31
(define (tree-map f t)
  (cond ((null? t) '())
        ((list? t) (append (list (tree-map f (car t)))
                           (tree-map f (cdr t))))
        (else (f t))))

; Ex 2.32
(define (subsets s)
  (if (null? s)
      '(())
      (let ((others (subsets (cdr s)))
            (this (car s)))
        (append others
                (map (lambda (i) (cons this i)) others)))))

;;; 2.2.3 Sequences as conventional interfaces

; accumulate is foldr

(define enumerate-tree tree-fringe)
(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ 1 a) b))))

(define (sum-odd-squares tree)
  (foldr + 0
         (map square
              (filter odd?
                      (enumerate-tree tree)))))

(define (even-fibs n)
  (foldr cons '()
         (filter even?
                 (map fib (enumerate-interval 0 n)))))

; Ex 2.33
(define (ex-map p seq)
  (foldr (lambda (i is) (cons (p i) is)) '() seq))

(define (ex-append s1 s2)
  (foldr cons s2 s1))

(define (ex-length seq)
  (foldr (lambda (_ n) (+ n 1)) 0 seq))

; Ex 2.34
(define (horner-eval x coefficient-sequence)
  (foldr (lambda (this-coeff higher-terms)
           (+ this-coeff (* x higher-terms)))
         0
         coefficient-sequence))

; Ex 2.35
; Hmm, how to do this as a foldr?
; (define (count-leaves t)
;   (foldr ? ? (map ? ?)))
; idea: map recursive call to count-leaves to reduce each
; list to a number of leaves it has and each atom to 1.
(define (count-leaves2 t)
  (foldr + 0
         (map (lambda (i)
                (if (list? i)
                    (count-leaves i)
                    1))
              t)))

; Ex 2.36
(define accumulate2 foldr)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate2 op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Ex 2.37
(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m_i)  (dot-product m_i v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m_i) (matrix-*-vector n m_i)) m)))

; Ex 2.38
; they should be associative? order of operations does not matter.


; list all pairs 0 < j < i <= n
(define (pairs-under n)
  (foldr append
         '()
         (map (lambda (i)
                (map (lambda (j)
                       (list i j))
                     (enumerate-interval 1 (- i 1))))
              (enumerate-interval 1 n))))

(define (flat-map proc seq)
  (foldr append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (first pair) (second pair))))

(define (make-pair-sum pair)
  (let ((a (first pair))
        (b (second pair)))
    (list a b (+ a b))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (pairs-under n))))

; permutations
(define (permutations s)
  (if (null? s)
      '(())
      (flat-map (lambda (x)
                  (map (lambda (p) (cons x p))
                       (permutations (remove x s))))
                s)))

; Ex. 2.40 see pairs-under above

;; Ex. 2.41
;(define (triples-under n)
;  (foldr append '()
;         (foldr append '() 
;                (map (lambda (j)
;                       (map (lambda (k)
;                              (list i j k))
;                            (enumerate-interval 1 (- j 1))))
;                     (enumerate-interval 1 (- i 1))))
;              (enumerate-interval 1 n)))
;
;(define (sum= s)
;  (lambda (l)
;    (= (apply + l) s)))
;
;(define (triples-with-sum-of s n)
;  (filter (sum= s)
;          (triples-under n)))

; 2.3 Symbolic Data
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (augend exp) var)
                   (deriv (addend exp) var)))
        ((product? exp)
         (let ([m1 (multiplicand exp)]
               [m2 (multiplier exp)])
           (make-sum (make-product m2 (deriv m1 var))
                     (make-product m1 (deriv m2 var)))))
        ((exponentiation? exp)
         (let ([base (exponent-base exp)]
               [exponent (exponent-exponent exp)])
           (make-product exponent
                         (make-product (make-exponent base (make-sum exponent -1))
                                       (deriv base var)))))
        (else (error "Unknown expression type in deriv" exp))))

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2) (eq? v1 v2))
(define (sum? exp) (and (cons? exp) (eq? (car exp) '+)))
(define (product? exp) (and (cons? exp) (eq? (car exp) '*)))

(define (make-sum exp1 exp2) (list '+ exp1 exp2))
(define augend cadr)
(define (addend exp)
  (let ([addend-part (cddr exp)])
    (if (null? (cdr addend-part))
        (car addend-part)
        (cons '+ addend-part))))
  
(define (make-product exp1 exp2) (list '* exp1 exp2))
(define multiplicand cadr)
(define (multiplier exp)
  (let ([multiplier-part (cddr exp)])
    (if (null? (cdr multiplier-part))
        (car multiplier-part)
        (cons '* multiplier-part))))

(define (simplify exp)
  (cond [(exponentiation? exp) 
         (let ([base (simplify (exponent-base exp))]
               [exponent (simplify (exponent-exponent exp))])
           (cond [(eq? base 1) 1]
                 [(eq? exponent 0) 1]
                 [(eq? exponent 1) base]
                 [else (make-exponent base exponent)]))]
        [(product? exp)
         (let ([m1 (simplify (multiplier exp))]
               [m2 (simplify (multiplicand exp))])
           (cond [(eq? m1 0) 0]
                 [(eq? m2 0) 0]
                 [(eq? m1 1) m2]
                 [(eq? m2 1) m1]
                 [(and (number? m1) (number? m2)) (* m1 m2)]
                 [else (make-product m1 m2)]))]
        [(sum? exp)
         (let ([a1 (simplify (addend exp))]
               [a2 (simplify (augend exp))])
           (cond [(eq? a1 0) a2]
                 [(eq? a2 0) a1]
                 [(and (number? a1) (number? a2)) (+ a1 a2)]
                 [(eq? a1 a2) (make-product 2 a1)]
                 [else (make-sum a1 a2)]))]
        [(variable? exp) exp]
        [(number? exp) exp]
        [else (error "invalid exp" exp)]))
  
  ; Exp 2.56
  (define (exponentiation? e)
    (and (cons? e) (eq? (car e) '**)))
  (define (make-exponent b n)
    (list '** b n))
  
  (define exponent-base cadr)
  (define exponent-exponent caddr)
  
  ;; Ex 2.58
  (define operations '(** * +))
  
  (define (find-rest atom lst)
    (cond [(null? lst) nil]
          [(eq? (car lst) atom) (cdr lst)]
          [else (find-rest atom (cdr lst))]))
  
  (define (highest-operation exp)
    (define (iter exp ops)
      (let* ([op (car exp)]
             
  
 )