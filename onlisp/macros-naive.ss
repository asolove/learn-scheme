#lang scheme

(require macro-debugger/stepper
         scheme/dict)

;; On Lisp
(define (memoize fn)
  (let ([answers (make-hash)])
    (lambda (a . args)
      (let* ([args (cons a args)]
             [no-answer (gensym)]
             [answer (dict-ref answers args no-answer)])
        (if (eq? answer no-answer)
            (let ([answer (apply fn args)])
              (dict-set! answers args answer)
              answer)
            answer)))))

(define (compose fn . fns)
  (let ([fns (reverse (cons fn fns))])
    (lambda (arg . args)
      (let ([args (cons arg args)])
        (foldl (lambda (arg fn)
                 (fn arg))
               (apply (car fns) args)
               (cdr fns))))))
                    
(define (and/f fn . fns)
  (let ([fns (cons fn fns)])
    (lambda (arg . args)
      (let ([args (cons arg args)])
        (and (map (apply-to args) fn))))))

(map (and/f odd? positive?)
     '(-2 -1 0 1 2 3))
    
        
(define (seq n)
  (if (< n 1)
      1
      (+ (seq (- n 1)) (seq (- n 2)))))

(define short (memoize seq))
    
        