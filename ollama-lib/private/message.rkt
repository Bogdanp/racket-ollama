#lang racket/base

(require json/to-jsexpr)

(provide
 (struct-out message)
 make-message)

(struct message (role content images tool-calls)
  #:transparent
  #:methods gen:to-jsexpr
  [(define-struct->jsexpr message
     #:convention snake_case-convention)])

(define (make-message
         #:role [role 'user]
         #:images [images null]
         content)
  (message
   #;role role
   #;content content
   #;images (map bytes->string/utf-8 images)
   #;tool-calls null))
