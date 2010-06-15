#lang scheme

(define make-tree list)
(define entry car)
(define left cadr)
(define right caddr)
(define (fringe? s)
  (and (null? (left s)) (null? (right s))))

(define (adjoin-binary-tree x set) 
  (cond ((null? set) (make-tree x '() '())) 
        ((= x (entry set)) set) 
        ((< x (entry set)) 
         (make-tree (entry set) 
                    (adjoin-set x (left set)) 
                    (right set))) 
        ((> x (entry set)) 
         (make-tree (entry set) 
                    (left set) 
                    (adjoin-set x (right set))))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((< (entry s1) (entry s2))
         (make-tree (entry s2)
                    (union-set s1 (left s2))
                    (right s2)))
        ((> (entry s1) (entry s2))
         (make-tree (entry s1)
                    (union-set (right s1) s2)
                    (left s1)))))

(define (tree->list-1 tree) 
  (if (null? tree) 
      '() 
      (append (tree->list-1 (left tree)) 
              (cons (entry tree) 
                    (tree->list-1 (right tree))))))

(define (tree->list-2 tree) 
  (define (copy-to-list tree result-list) 
    (if (null? tree) 
        result-list 
        (copy-to-list (left tree) 
                      (cons (entry tree) 
                            (copy-to-list (right tree) 
                                          result-list))))) 
  (copy-to-list tree '()))

(define (big-tree n)
  (if (zero? n) 
      '()
      (adjoin-set n (big-tree (- n 1)))))
  
(define (list->tree elements) 
  (car (partial-tree elements (length elements)))) 
(define (partial-tree elts n) 
  (if (= n 0) 
      (cons '() elts) 
      (let ((left-size (quotient (- n 1) 2))) 
        (let ((left-result (partial-tree elts left-size))) 
          (let ((left-tree (car left-result)) 
                (non-left-elts (cdr left-result)) 
                (right-size (- n (+ left-size 1)))) 
            (let ((this-entry (car non-left-elts)) 
                  (right-result (partial-tree (cdr non-left-elts) 
                                              right-size))) 
              (let ((right-tree (car right-result)) 
                    (remaining-elts (cdr right-result))) 
                (cons (make-tree this-entry left-tree right-tree) 
                      remaining-elts))))))))




(define nil '())


;; Huffman encoding
(define (make-leaf sym w)
  (list 'leaf sym w))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define leaf-symbol cadr)
(define leaf-weight caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)
(define right-branch cadr)
(define (symbols tree)
  (if (leaf? tree)
      (list (leaf-symbol tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (leaf-weight tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch 
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (leaf-symbol next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  ((cond [(= bit 0) left-branch]
         [(= bit 1) right-branch]
         [else error])
   branch))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; 2.67
(define sample-tree 
  (make-code-tree (make-leaf 'A 4) 
                  (make-code-tree 
                   (make-leaf 'B 2) 
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1))))) 
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)) 
; (A D A B B C A)

; Ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond [(and (leaf? tree) (eq? (leaf-symbol tree) sym))
         '()]
        [(member sym (symbols (left-branch tree)))
         (cons 0 (encode-symbol sym (left-branch tree)))]
        [(member sym (symbols (right-branch tree)))
         (cons 1 (encode-symbol sym (right-branch tree)))]
        [else (error "Letter can't be encoded in tree" sym)]))
        
; Ex 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (single? l)
  (and (cons? l) (null? (cdr l))))

(define (successive-merge leaf-set)
  (if (single? leaf-set) 
      (car leaf-set)
      (let-values ([(first-two rest) (split-at leaf-set 2)])
        (successive-merge 
         (adjoin-set (apply make-code-tree first-two)
                     rest)))))

; 2.71 
; Interesting. 2^n = Sum(2^1 ... + 2^(n-1)) + 1, so the tree balances by always branching right
; It takes 1 bit for the most common symbol, and n bits for the most. This distribution becomes
; very inefficient for n > 8 in comparison with plain ASCII

(define (square x) (* x x))

; 2.4 Multiple representations for data
(define (attach-tag type-tag contents)
  (cons type-tag contents)) 
(define (type-tag datum) 
  (if (pair? datum) 
      (car datum) 
      (error "Bad tagged datum -- TYPE-TAG" datum))) 
(define (contents datum) 
  (if (pair? datum) 
      (cdr datum) 
      (error "Bad tagged datum -- CONTENTS" datum))) 

(define (rectangular? z) 
  (eq? (type-tag z) 'rectangular)) 
(define (polar? z) 
  (eq? (type-tag z) 'polar)) 

(define (real-part-rectangular z) (car z)) 
(define (imag-part-rectangular z) (cdr z)) 
(define (magnitude-rectangular z) 
  (sqrt (+ (square (real-part-rectangular z)) 
           (square (imag-part-rectangular z))))) 
(define (angle-rectangular z) 
  (atan (imag-part-rectangular z) 
        (real-part-rectangular z))) 
(define (make-from-real-imag-rectangular x y) 
  (attach-tag 'rectangular (cons x y))) 
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular 
              (cons (* r (cos a)) (* r (sin a))))) 


(define (real-part-polar z) 
  (* (magnitude-polar z) (cos (angle-polar z)))) 
(define (imag-part-polar z) 
  (* (magnitude-polar z) (sin (angle-polar z)))) 
(define (magnitude-polar z) (car z)) 
(define (angle-polar z) (cdr z)) 
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar 
               (cons (sqrt (+ (square x) (square y))) 
                     (atan y x)))) 
(define (make-from-mag-ang-polar r a) 
  (attach-tag 'polar (cons r a))) 

(define (real-part z) 
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z))) 
        ((polar? z) 
         (real-part-polar (contents z))) 
        (else (error "Unknown type -- REAL-PART" z)))) 
(define (imag-part z) 
  (cond ((rectangular? z) 
         (imag-part-rectangular (contents z))) 
        ((polar? z) 
         (imag-part-polar (contents z))) 
        (else (error "Unknown type -- IMAG-PART" z)))) 
(define (magnitude z) 
  (cond ((rectangular? z) 
         (magnitude-rectangular (contents z))) 
        ((polar? z) 
         (magnitude-polar (contents z))) 
        (else (error "Unknown type -- MAGNITUDE" z)))) 
(define (angle z) 
  (cond ((rectangular? z) 
         (angle-rectangular (contents z))) 
        ((polar? z) 
         (angle-polar (contents z))) 
        (else (error "Unknown type -- ANGLE" z)))) 


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

(define (make-from-real-imag x y) 
  (make-from-real-imag-rectangular x y)) 
(define (make-from-mag-ang r a) 
  (make-from-mag-ang-polar r a)) 
