#lang scheme

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    ((_ e1 e2 ...)
     (let ([t e1])
       (if t 
           t
           (my-or e2 ...))))))

(define-syntax my-or2
  (lambda (x)
    (syntax-case x ()
      [(_) #'#f]
      [(_ e) #'e]
      [(_ e1 e2 ...)
       #'(let ([t e1]) (if t t (or e2 ...)))])))

(define-syntax my-let
  (lambda (x)
    (define ids?
      (lambda (ls)
        (or (null? ls)
            (and (identifier? (car ls))
                 (ids? (cdr ls))))))
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (ids? #'(i ...))
       #'((lambda (i ...) b1 b2 ...) e ...)])))

(define-syntax my-cond
  (syntax-rules (else)
    [(_ ()) #f]
    [(_ (else r1 r2 ...)) (begin r1 r2 ...)]
    [(_ (c1 r1 r2 ...)) (if c1 (begin r1 r2 ...))]
    [(_ (c1 r1 r2 ...) e2 ...)
     (if c1
         (begin r1 r2 ...)
         (cond e2 ...))]))


(define-syntax my-cond3
  (lambda (x)
    (syntax-case x ()
      [(_ c1 c2 ...)
       (let f ([c1 #'c1] [cmore #'(c2 ...)])
         (if (null? cmore)
             (syntax-case c1 (else =>)
               [(else e1 e2 ...) #'(begin e1 e2 ...)]
               [(e0) #'(let ([t e0]) (if t t))]
               [(e0 => e1) #'(let ([t e0]) (if t (e1 t)))]
               [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...))])
             (with-syntax ([rest (f (car cmore) (cdr cmore))])
               (syntax-case c1 (=>)
                 [(e0) #'(let ([t e0]) (if t t rest))]
                 [(e0 => e1) #'(let ([t e0]) (if t (e1 t) rest))]
                 [(e0 e1 e2 ...)
                  #'(if e0 (begin e1 e2 ...) rest)]))))])))