#lang scheme/base

(define-controller (review-post request slug)
  `(html (head (title ,slug))
         (body (h1 "You are viewing " ,(format "~s" slug))
               (p "Some content should go here"))))