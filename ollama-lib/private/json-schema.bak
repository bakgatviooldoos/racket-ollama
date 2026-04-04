#lang racket/base

(require racket/hash
         racket/match
         racket/symbol)

(provide
 (all-defined-out))

(define Boolean (hasheq 'type "boolean"))
(define Null    (hasheq 'type "null"))
(define Number  (hasheq 'type "number"))
(define String  (hasheq 'type "string"))

(define (Array type)
  (hasheq
   'type "array"
   'items type))

(define (Enum . options)
  (hasheq
   'type "string"
   'enum options))

(define (Object* kvs)
  (hasheq
   'type "object"
   'required (map (compose1 symbol->immutable-string car) kvs)
   'properties (for/hasheq ([kv (in-list kvs)])
                 (match-define (cons k v) kv)
                 (values k v))))

(define (Or . types)
  (apply
   #:combine/key
   (lambda (k v1 v2)
     (case k
       [(type)
        (if (pair? v1)
            (cons v2 v1)
            (list v2 v1))]
       [else v2]))
   hash-union types))

(define (Optional type)
  (Or Null type))

(define (with-description type description)
  (hash-set type 'description description))
