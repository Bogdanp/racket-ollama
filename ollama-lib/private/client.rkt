#lang racket/base

(require net/http-easy
         racket/mutable-treelist
         racket/string
         racket/treelist
         struct-define
         threading
         "json.rkt"
         "message.rkt"
         "tool.rkt")

(provide
 make-ollama-client
 ollama-client?
 ollama-start-chat)

(define timeouts
  (make-timeout-config
   #:request (* 5 60)))

(struct ollama-client (auth session ~endpoint))

(define (make-ollama-client
         #:auth [auth (λ (_url headers params)
                        (values headers params))]
         [root "http://127.0.0.1:11434"])
  (ollama-client
   #;auth auth
   #;session (current-session)
   #;~endpoint (lambda args
                 (format "~a/~a" root (string-join args "/")))))

(define (ollama-start-chat
         #:options [options (hasheq)]
         #:format [output-format #f]
         #:tools [tools #f]
         c model str-or-message)
  (struct-define ollama-client c)
  (let loop ([messages (treelist (ensure-message str-or-message))]
             [output-format output-format]
             [tools tools])
    (define resp
      (~> (session-request
           #:method 'post
           #:stream? #t
           #:auth auth
           #:json (hasheq
                   'model model
                   'stream #t
                   'options options
                   'messages (->jsexpr messages)
                   'tools (if tools
                              (tools->jsexpr tools)
                              (json-null))
                   'format (if output-format
                               (->jsexpr output-format)
                               (json-null)))
           #:timeouts timeouts
           session (~endpoint "api" "chat"))
          (check-response 'ollama-chat _)))
    (let ([messages (treelist-copy messages)])
      (values
       (lambda ()
         (define data (read-json (response-output resp)))
         (cond
           [(eof-object? data)
            (begin0 eof
              (response-close! resp))]
           [else
            (define llm-message (hash-ref data 'message))
            (begin0 data
              (mutable-treelist-add! messages llm-message))]))
       (lambda (#:format [output-format output-format] ;; noqa
                #:tools [tools tools] ;; noqa
                next-message)
         (if (list? next-message)
             (mutable-treelist-append! messages (list->treelist next-message))
             (mutable-treelist-add! messages (ensure-message next-message)))
         (loop (mutable-treelist-snapshot messages) output-format tools))))))

(define (ensure-message str-or-message)
  (if (message? str-or-message)
      str-or-message
      (make-message str-or-message)))

(define (check-response who resp [ok '(200)])
  (begin0 resp
    (unless (memv (response-status-code resp) ok)
      (error who "request failed~n  status: ~s~n  body: ~e"
             (response-status-code resp)
             (response-body resp)))))
