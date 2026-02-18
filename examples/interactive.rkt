#lang racket/base

(require expeditor
         ollama
         racket/string)

(define-tool-definer define-tool
  #:getter get-tools
  #:caller call-tool)

(define-tool (get-current-temperature
              [location : (with-description String "The user's current location")]
              [units u : (Enum "celsius" "fahrenheit")])
  (format "19 ~a" u))

(define (main model)
  (parameterize ([current-expeditor-reader read-line])
    (call-with-expeditor
     (lambda (prompt)
       (let ([prompt (lambda ()
                       (define res (prompt))
                       (cond
                         [(eof-object? res)
                          (exit)]
                         [(equal? (string-trim res) "")
                          (prompt)]
                         [else res]))])
         (define c (make-ollama-client))
         (define-values (more continue)
           (ollama-start-chat
            #:tools (get-tools)
            #:options (hasheq 'num_ctx (* 128 1024))
            c model (prompt)))
         (let loop ()
           (define tool-calls
             (for/fold ([tool-calls null])
                       ([data (in-producer more eof)])
               (define message (hash-ref data 'message))
               (define content (hash-ref message 'content))
               (define calls (hash-ref message 'tool_calls null))
               (cond
                 [(null? calls)
                  (begin0 tool-calls
                    (display content))]
                 [else
                  (append tool-calls calls)])))
           (cond
             [(null? tool-calls)
              (newline)
              (set!-values
               (more continue)
               (continue (prompt)))
              (loop)]
             [else
              (define call-messages
                (for/list ([data (in-list tool-calls)])
                  (make-message
                   #:role 'tool
                   (call-tool data))))
              (set!-values
               (more continue)
               (continue call-messages))
              (loop)])))))))

(module+ main
  (require racket/cmdline)
  (define model
    (command-line
     #:args [model]
     model))
  (main model))
