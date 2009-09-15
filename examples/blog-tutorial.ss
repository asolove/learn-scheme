#lang web-server/insta
; -----------------------------------
; Models
; -----------------------------------

; Post
(define-struct post (title body comments) #:mutable)

(define (post-insert-comment! post comment)
  (set-post-comments! post (append (post-comments post) (list comment))))

; Blog 
(define-struct blog (posts) #:mutable)
(define BLOG (make-blog 
              (list (make-post "First post!"
                               "Entry body!"
                               '("What is wrong with you?"))
                    (make-post "Second post!"
                               "This post is exetremely interesting."
                               '()))))

(define (blog-insert-post! blog post)
  (set-blog-posts! blog (append (blog-posts blog) post)))

; ___________________________________
; Views
; -----------------------------------
(define (render-post post make-url)
  (local [(define (view-post-handler request)
            (render-post-detail-page post request))]
    `(div ((class "post")) 
          (a ((href ,(make-url view-post-handler)))
             (h2 ,(post-title post)))
          (p ,(post-body post))
          (div ,(number->string (length (post-comments post)))
               " comment(s)"))))
  
(define (render-post-comment comment)
  `(blockquote ,comment))

(define (render-posts make-url)
  (local [(define (render-post/make-url post)
            (render-post post make-url))]
    `(div ((class "posts"))
          ,@(map render-post/make-url (blog-posts BLOG)))))

; View helpers
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))
(define (render-as-item fragment)
  `(li ,fragment))

; ---------------------
; Controller functions
; ---------------------
(define (start request)
  (render-blog-page request))

(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body  bindings)))

(define (parse-post bindings)
  (make-post (extract-binding/single 'title bindings)
             (extract-binding/single 'body  bindings)
             (list)))

(define (render-blog-page request)
  (local [(define (response-generator make-url)
            `(html (head (title "My blog"))
                   (body (h1 "My blog")
                         ,(render-posts make-url)
                         (form ((action ,(make-url insert-post-handler)))
                          (input ((name "title")))
                          (input ((name "body")))
                          (input ((type "submit")))))))
          
          (define (insert-post-handler request)
            (blog-insert-post! BLOG (parse-post (request-bindings request)))
            (render-blog-page request))]
    
    (send/suspend/dispatch response-generator)))

(define (render-post-detail-page a-post request)
  (local [(define (response-generator make-url)
            `(html (head (title "Post details"))
                   (body
                    (h1 "Post details")
                    (h2 ,(post-title a-post))
                    (p ,(post-body a-post))
                    ,(render-as-itemized-list 
                      (post-comments a-post))
                    (form ((action
                            ,(make-url insert-comment-handler)))
                          (input ((name "comment")))
                          (input ((type "submit")))))))
          
          (define (parse-comment bindings)
            (extract-binding/single 'comment bindings))
          
          (define (insert-comment-handler a-request)
            (post-insert-comment!
             a-post
             (parse-comment (request-bindings a-request)))
            (render-post-detail-page a-post request))]
    
    (send/suspend/dispatch response-generator)))
                   