#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         struct-define
         "json-schema.rkt")

(provide
 (all-from-out "json-schema.rkt")
 (struct-out tool-arg-info)
 (struct-out tool-info)

 :
 Object
 define-tool-definer
 tools->jsexpr)

(define-syntax (: stx)
  (raise-syntax-error ': "may only be used within a define-tool or an Object form" stx))

(define-syntax (Object stx)
  (syntax-parse stx
    #:literals (:)
    [(_ [k : t] ...+)
     #'(Object* (list (cons 'k t) ...))]))

(struct tool-arg-info (id label type))
(struct tool-info (id proc args))

(begin-for-syntax
  (define-syntax-class arg
    #:literals (:)
    (pattern [id:id : type:expr] #:attr label #'id)
    (pattern [label:id id:id : type:expr])))

(define (do-call-tool tools data)
  (let/ec esc
    (define func (hash-ref data 'function))
    (define name (string->symbol (hash-ref func 'name)))
    (define arguments (hash-ref func 'arguments))
    (struct-define tool-info (hash-ref tools name (λ () (esc (format "error: tool ~a does not exist." name)))))
    (define arg-vals
      (for/list ([arg (in-list args)])
        (struct-define tool-arg-info arg)
        (hash-ref arguments label (λ () (esc (format "error: tool '~a' requires '~a' as an argument." name label))))))
    (apply proc arg-vals)))

(define-syntax (define-tool-definer stx)
  (syntax-parse stx
    [(_ definer:id #:getter get-tools:id #:caller call-tool:id)
     #'(begin
         (define tools (make-hasheq))
         (define-syntax (definer stx)
           (syntax-parse stx
             [(_ (id:id arg:arg (... ...)) e ...+)
              #'(begin
                  (define (id arg.id (... ...)) e (... ...))
                  (define info
                    (tool-info
                     #;id 'id
                     #;proc id
                     #;args (list
                             (tool-arg-info
                              #;id 'arg.id
                              #;label 'arg.label
                              #;type arg.type) (... ...))))
                  (hash-set! tools 'id info))]))
         (define (get-tools)
           (hash-copy tools))
         (define (call-tool data)
           (do-call-tool tools data)))]))

(define (tool->jsexpr t)
  (struct-define tool-info t)
  (hasheq
   'type "function"
   'function
   (hasheq
    'name (symbol->string id)
    'parameters
    (Object*
     (for/list ([arg (in-list args)])
       (struct-define tool-arg-info arg)
       (cons label type))))))

(define (tools->jsexpr tools)
  (map tool->jsexpr (hash-values tools)))
