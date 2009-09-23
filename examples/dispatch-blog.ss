#lang scheme/base

(require (planet untyped/dispatch))
  
(define-site blog
  ([(url "/") index]
   [(url "/posts/" (string-arg)) review-post]
   [(url "/archive/" (integer-arg) "/" (integer-arg))
    review-archive]))

(serve/dispatch site)

(provide (site-out blog))