#lang scheme

;; Defines macros, the declarations can have `keyword: <kwd>' to
;; specify keywords in the resulting macro, and `capture: <id>' to
;; specify names that are inserted unhygienically.  See examples
;; below.
(define-syntax defmac
  (syntax-rules ()
    [(defmac (name . xs) decl ... body)
     (defmac-decls name xs (decl ...) () () body)]))

;; helper for defmac -- collects keyword & capture declarations
(define-syntax defmac-decls
  (syntax-rules (keyword: capture:)
    [(defmac-decls name xs (keyword: key* more ...) (key ...) caps body)
     (defmac-decls name xs (more ...) (key ... key*) caps body)]
    [(defmac-decls name xs (capture: cap* more ...) keys (cap ...) body)
     (defmac-decls name xs (more ...) keys (cap ... cap*) body)]
    [(defmac-decls name xs () (key ...) (cap ...) body)
     (define-syntax (name stx)
       (syntax-case stx (key ...)
         [(name . xs)
          (with-syntax ([cap (datum->syntax-object stx 'cap stx)] ...)
            (syntax body))]))]))


(defmac (aif expr if-clause then-clause) capture: it
  (let ([it expr])
    (if it
        if-clause
        else-clause)))