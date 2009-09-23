#lang scheme/base
  
(require (planet untyped/dispatch)
         "controllers/posts.ss"
         "site.ss")

(serve/dispatch site)