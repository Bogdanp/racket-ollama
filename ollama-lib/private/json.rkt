#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/struct-info
                     racket/syntax
                     syntax/parse/pre)
         json
         racket/generic
         racket/mutable-treelist
         racket/symbol
         racket/treelist)

(provide
 (all-from-out json)
 gen:to-jsexpr
 to-jsexpr?
 ->jsexpr
 define-struct->jsexpr)

(define-generics to-jsexpr
  {->jsexpr to-jsexpr}
  #:defaults
  ([(lambda (v)
      (or (boolean? v)
          (string? v)
          (number? v)))
    (define ->jsexpr values)]
   [symbol?
    (define ->jsexpr symbol->immutable-string)]
   [(lambda (v)
      (or (null? v)
          (pair? v)))
    (define/generic recur ->jsexpr)
    (define (->jsexpr vs)
      (map recur vs))]
   [(lambda (v)
      (and (hash? v)
           (hash-eq? v)))
    (define/generic recur ->jsexpr)
    (define (->jsexpr ht)
      (for/hasheq ([(k v) (in-hash ht)])
        (values k (recur v))))]
   [treelist?
    (define/generic recur ->jsexpr)
    (define (->jsexpr vs)
      (for/list ([v (in-treelist vs)])
        (recur v)))]
   [mutable-treelist?
    (define/generic recur ->jsexpr)
    (define (->jsexpr vs)
      (for/list ([v (in-mutable-treelist vs)])
        (recur v)))]
   [path?
    (define ->jsexpr path->string)]))

(define-syntax (define-struct->jsexpr stx)
  (syntax-parse stx
    [(_ struct-id:id)
     #:with ->jsexpr (format-id #'struct-id "->jsexpr")
     #:with self #'self
     #:with recur #'recur
     (match-define (list _descr _ctor _pred accessors _mutators _parent)
       (extract-struct-info (syntax-local-value #'struct-id)))
     (with-syntax ([(fld+accessor ...)
                    (flatten
                     (for/list ([accessor (in-list accessors)])
                       (define field-id
                         (string->symbol
                          (regexp-replace*
                           #;pattern #rx"-"
                           #;input
                           (regexp-replace
                            #;pattern #rx"^[^-]+-"
                            #;input (symbol->string (syntax-e accessor))
                            #;insert "")
                           #;insert "_")))
                       (with-syntax ([field-id field-id]
                                     [accessor accessor])
                         (list #''field-id #'(recur (accessor self))))))])
       #'(begin
           (define/generic recur ->jsexpr)
           (define (->jsexpr self)
             (hasheq fld+accessor ...))))]))
