#lang racket/base

(require "json.rkt")

(provide
 (struct-out message)
 make-message)

(struct message (role content images tool-calls)
  #:methods gen:to-jsexpr
  [(define-struct->jsexpr message)])

(define (make-message
         #:role [role 'user]
         #:images [images null]
         content)
  (message
   #;role role
   #;content content
   #;images (map bytes->string/utf-8 images)
   #;tool-calls null))
