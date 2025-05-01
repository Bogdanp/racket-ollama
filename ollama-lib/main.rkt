#lang racket/base

(require json
         net/http-easy
         racket/contract/base
         "private/client.rkt"
         "private/json-schema.rkt"
         "private/message.rkt"
         "private/tool.rkt")

(provide
 ollama-client?
 (contract-out
  [make-ollama-client
   (->* []
        [string?
         #:auth auth-procedure/c]
        ollama-client?)]
  [ollama-start-chat
   (->* [ollama-client? string? (or/c string? message?)]
        [#:options jsexpr?
         #:format (or/c #f 'json jsexpr?)
         #:tools (or/c #f (hash/c symbol? tool-info?))]
        (values
         chat-response/c
         chat-continuation/c))])

 message?
 (contract-out
  [make-message
   (->* [string?]
        [#:role (or/c 'system 'user 'tool)
         #:images (listof bytes?)]
        message?)])

 chat-response/c
 chat-continuation/c

 :
 tool-info?
 define-tool-definer
 (all-from-out "private/json-schema.rkt"))

(define chat-response/c
  (-> (or/c jsexpr? eof-object?)))

(define chat-continuation/c
  (->* [(or/c string? message? (listof message?))]
       [#:format (or/c #f 'json jsexpr?)
        #:tools (or/c #f (hash/c symbol? tool-info?))]
       (values
        chat-response/c
        (recursive-contract chat-continuation/c))))
