#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         struct-define
         "json-schema.rkt"
         "json.rkt")

(provide
 (all-from-out "json-schema.rkt")
 (struct-out tool-arg-info)
 (struct-out tool-info)

 :
 Object
 define-tool-definer
 tools->jsexpr

 current-call-data
 (struct-out exn:fail:tool)
 (struct-out exn:fail:tool:not-found)
 (struct-out exn:fail:tool:call))

(define current-call-data
  (make-parameter #f))

(struct exn:fail:tool exn:fail (call-data)
  #:methods gen:to-jsexpr
  [(define (->jsexpr e)
     (struct-define exn:fail:tool e)
     (hash-set call-data 'error (exn-message e)))])
(struct exn:fail:tool:not-found exn:fail:tool ())
(struct exn:fail:tool:call exn:fail:tool ())

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

(define (do-call-tool tools data)
  (define func (hash-ref data 'function))
  (define name-str (hash-ref func 'name))
  (define name (string->symbol name-str))
  (define arguments (hash-ref func 'arguments))
  (define info
    (hash-ref
     #;ht tools
     #;key name
     #;failure-result
     (lambda ()
       (raise
        (exn:fail:tool:not-found
         (format "tool '~a' does not exist" name)
         (current-continuation-marks)
         #;call-data data)))))
  (struct-define tool-info info)
  (define arg-vals
    (for/list ([arg (in-list args)])
      (struct-define tool-arg-info arg)
      (hash-ref
       #;ht arguments
       #;key label
       #;failure-result
       (lambda ()
         (raise
          (exn:fail:tool:call
           (format "tool '~a' requires '~a' as an argument" name label)
           (current-continuation-marks)
           #;call-data data))))))
  (define res
    (parameterize ([current-call-data data])
      (apply proc arg-vals)))
  (jsexpr->string
   (hash-set func 'result (->jsexpr res))))

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
