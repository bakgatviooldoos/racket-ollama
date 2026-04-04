#lang racket/base

(require json
         racket/hash
         racket/list
         racket/match
         racket/symbol)

(provide
 (all-defined-out))

(define Boolean (hasheq 'type "boolean"))
(define Null    (hasheq 'type "null"))
(define Number  (hasheq 'type "number"))
(define Integer (hasheq 'type "integer"))
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

(define (AllOf . types) (hasheq 'allOf types))
(define (AnyOf . types) (hasheq 'anyOf types))
(define (OneOf . types) (hasheq 'oneOf types))

(define (Not type) (hasheq 'not type))
(define (Const value) (hasheq 'const value))

(define (with-description type description)
  (hash-set type 'description description))

(define (with-validate/string type
          #:pattern [rx #f]
          #:min-length [min-length #f]
          #:max-length [max-length #f]
          #:format [format #f])
  (let* ([type (if (not rx) type (hash-set type 'pattern (if (regexp? rx) (object-name rx) rx)))]
         [type (if (not min-length) type (hash-set type 'minLength min-length))]
         [type (if (not max-length) type (hash-set type 'maxLength max-length))]
         [type (if (not format) type (hash-set type 'format format))])
    type))

(define (with-validate/number type
          #:minimum [minimum #f]
          #:maximum [maximum #f]
          #:multiple-of [multiple-of #f]
          #:format [format #f])
  (let* ([type (if (not minimum) type (hash-set type 'minimum minimum))]
         [type (if (not maximum) type (hash-set type 'maximum maximum))]
         [type (if (not multiple-of) type (hash-set type 'multipleOf multiple-of))]
         [type (if (not format) type (hash-set type 'format format))])
    type))

(define (with-validate/array type
          #:prefix-items [prefix-items #f]
          #:not-items? [not-items? #f]
          #:contains [contains #f]
          #:min-contains [min-contains #f]
          #:max-contains [max-contains #f]
          #:min-items [min-items #f]
          #:max-items [max-items #f]
          #:unique-items? [unique-items? #f])
  (let* ([type (if (not prefix-items) type (hash-set type 'prefixItems prefix-items))]
         [type (if (not not-items?) type (hash-set type 'items #f))]
         [type (if (not min-items) type (hash-set type 'minItems min-items))]
         [type (if (not max-items) type (hash-set type 'maxItems max-items))]
         [type (if (not contains) type (hash-set type 'contains contains))]
         [type (if (not min-contains) type (hash-set type 'minContains min-contains))]
         [type (if (not max-contains) type (hash-set type 'maxContains max-contains))]
         [type (if (not unique-items?) type (hash-set type 'uniqueItems #t))])
    type))

(define (with-validate/object type
          #:additional-properties? [additional-properties? #t]
          #:min-properties [min-properties #f]
          #:max-properties [max-properties #f])
  (let* ([type (if additional-properties? type (hash-set type 'additionalProperties #f))]
         [type (if (not min-properties) type (hash-set type 'minProperties min-properties))]
         [type (if (not max-properties) type (hash-set type 'maxProperties max-properties))])
    type))

(define (multiple-of? x y)
  (integer? (/ x y)))

(define-syntax-rule
  (implies p q ...)
  (or (not p) (and q ...)))

(define is-a?
  (match-lambda**
    [{_ (hash)} #true]
    
    [{v (hash* ['not schema])}
     (not (is-a? v schema))]

    [{v (hash* ['const value])}
     (equal? value v)]
    
    [{v (hash* ['type "null"]
               #:open)}
     (eq? (json-null) v)]

    [{v (hash* ['type "boolean"]
               #:open)}
     (boolean? v)]
    
    [{(? number? v)
      (hash* ['type "number"]
             ['minimum minimum #:default -inf.0]
             ['maximum maximum #:default +inf.0]
             ['multipleOf multiple-of #:default #f]
             #:open)}
     (and
      (<= minimum v maximum)
      (implies multiple-of
               (multiple-of? v multiple-of)))]

    [{(? integer? v)
      (hash* ['type "integer"]
             ['minimum minimum #:default -inf.0]
             ['maximum maximum #:default +inf.0]
             ['multipleOf multiple-of #:default #f]
             #:open)}
     (and
      (<= minimum v maximum)
      (implies multiple-of
               (multiple-of? v multiple-of)))]
    
    [{(? string? v)
      (hash* ['type "string"]
             ['enum options #:default #f]
             ['pattern rx #:default #f]
             ['minLength min-length #:default -inf.0]
             ['maxLength max-length #:default +inf.0]
             #:open)}
     (and
      (<= min-length (string-length v) max-length)
      (implies options
               (member v options))
      (implies rx
               (regexp-match? (regexp rx) v)))]

    [{(? list? v)
      (hash* ['type "array"]
             ['items items]
             ['prefixItems prefix-items #:default #f]
             ['contains contains #:default #f]
             ['minContains min-contains #:default 1]
             ['maxContains max-contains #:default +inf.0]
             ['minItems min-items #:default 0]
             ['maxItems max-items #:default +inf.0]
             ['uniqueItems unique-items? #:default #f]
             #:open)}
     (and
      (implies items
               (for/and ([item (in-list v)])
                 (is-a? item items))
               (<= min-items (length v) max-items))
      (implies prefix-items
               (for/and ([schema (in-list prefix-items)]
                         [item (in-list v)])
                 (is-a? item schema)))
      (implies contains
               (<= min-contains
                   (for/sum ([item (in-list v)])
                     (if (is-a? item contains) 1 0))
                   max-contains))
      (implies unique-items?
               (not (check-duplicates v))))]
    
    [{(? hash? v)
      (hash* ['type "object"]
             ['properties props]
             ['required required #:default null]
             ['additionalProperties additional-properties? #:default #t]
             ['minProperties min-properties #:default 0]
             ['maxProperties max-properties #:default +inf.0]
             #:open)}
     (and
      (for/and ([key (in-list required)])
        (hash-has-key? v key))
      (or
       additional-properties?
       (= (length required) (hash-count v)))
      (<= min-properties (hash-count v) max-properties)
      (for/and ([(key schema) (in-immutable-hash props)])
        (is-a? (hash-ref v key 'not-found) schema)))]
    
    [{v (and schema
             (hash* ['type (list types ...)]
                    #:open))}
     (for/or ([type (in-list (remove-duplicates types))])
       (is-a? v (hash-set schema 'type type)))]
    
    [{v (hash* ['allOf (list schemas ...)])}
     (for/and ([schema (in-list schemas)])
       (is-a? v schema))]

    [{v (hash* ['anyOf (list schemas ...)])}
     (for/or ([schema (in-list schemas)])
       (is-a? v schema))]

    [{v (hash* ['oneOf (list schemas ...)])}
     (for/fold ([valid 0]
                #:result (= 1 valid))
               ([schema (in-list schemas)]
                #:break (< 1 valid))
       (+ valid (if (is-a? v schema) 1 0)))]
    
    [{_ _} #false]))