#lang racket/base

(require json
         racket/hash
         racket/list
         racket/match
         racket/symbol)

(provide
  (all-defined-out))

(define None    #f)
(define Any     (hasheq))
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

(define (Not type) (hasheq 'not type))
(define (Const value) (hasheq 'const value))

(define (AllOf #:in [in Any] . types) (hash-set in 'allOf types))
(define (AnyOf #:in [in Any] . types) (hash-set in 'anyOf types))
(define (OneOf #:in [in Any] . types) (hash-set in 'oneOf types))

(define (Condition #:in [in Any] #:if cond #:then then #:else [else Any])
  (hash-set* in 'if cond 'then then 'else else))

(define (with-description type description)
  (hash-set type 'description description))

(define-syntax-rule
  (depends [e p] q ...)
  (cond [(not p) e] [else q ...]))

(define (with-string [type Any]
          #:pattern [rx #f]
          #:min-length [min-length #f]
          #:max-length [max-length #f]
          #:format [format #f])
  (let* ([type (depends [type rx] (hash-set type 'pattern (object-name rx)))]
         [type (depends [type min-length] (hash-set type 'minLength min-length))]
         [type (depends [type max-length] (hash-set type 'maxLength max-length))]
         [type (depends [type format] (hash-set type 'format (symbol->string format)))])
    type))

(define (with-number [type Any]
          #:minimum [minimum #f]
          #:maximum [maximum #f]
          #:multiple-of [multiple-of #f]
          #:format [format #f])
  (let* ([type (depends [type minimum] (hash-set type 'minimum minimum))]
         [type (depends [type maximum] (hash-set type 'maximum maximum))]
         [type (depends [type multiple-of] (hash-set type 'multipleOf multiple-of))]
         [type (depends [type format] (hash-set type 'format (symbol->string format)))])
    type))

(define (with-array [type Any]
          #:min-items [min-items #f]
          #:max-items [max-items #f]
          #:not-items? [not-items? #f]
          #:prefix-items [prefix-items #f]
          #:contains [contains #f]
          #:min-contains [min-contains #f]
          #:max-contains [max-contains #f]
          #:unique-items? [unique-items? #f])
  (let* ([type (depends [type min-items] (hash-set type 'minItems min-items))]
         [type (depends [type max-items] (hash-set type 'maxItems max-items))]
         [type (depends [type not-items?] (hash-set type 'items #f))]
         [type (depends [type prefix-items] (hash-set type 'prefixItems prefix-items))]
         [type (depends [type contains] (hash-set type 'contains contains))]
         [type (depends [type min-contains] (hash-set type 'minContains min-contains))]
         [type (depends [type max-contains] (hash-set type 'maxContains max-contains))]
         [type (depends [type unique-items?] (hash-set type 'uniqueItems #t))])
    type))

(define (with-object [type Any]
          #:required [required #f]
          #:min-properties [min-properties #f]
          #:max-properties [max-properties #f]
          #:pattern-properties [pattern-properties #f]
          #:additional-properties? [additional-properties? #t]
          #:dependent-required [dependent-required #f]
          #:dependent-schemas [dependent-schemas #f])
  (let* ([type (depends [type required] (hash-set type 'required required))]
         [type (depends [type min-properties] (hash-set type 'minProperties min-properties))]
         [type (depends [type max-properties] (hash-set type 'maxProperties max-properties))]
         [type (depends [type pattern-properties]
                 (hash-set type 'patternProperties
                           (for/hasheq ([(rx schema) (in-immutable-hash pattern-properties)])
                             (values (string->symbol (object-name rx)) schema))))]
         [type (depends [type (not additional-properties?)] (hash-set type 'additionalProperties #f))]
         [type (depends [type dependent-required]
                 (hash-set type 'dependentRequired dependent-required))]
         [type (depends [type dependent-schemas]
                 (hash-set type 'dependentSchemas dependent-schemas))])
    type))

(define-syntax-rule
  (implies p q ...)
  (or (not p) (and q ...)))

(define json-is?
  (match-lambda**
    [{_ #f} #f]
    
    [{_ (hash #:closed)} #t]
    
    [{v (hash* ['not schema]
               #:closed)}
     (not (json-is? v schema))]

    [{v (hash* ['const value]
               #:closed)}
     (equal? value v)]
    
    [{v (hash* ['allOf (list schemas ...)]
               #:rest u)}
     (and
      (json-is? v u)
      (for/and ([schema (in-list schemas)])
        (json-is? v schema)))]

    [{v (hash* ['anyOf (list schemas ...)]
               #:rest u)}
     (and
      (json-is? v u)
      (for/or ([schema (in-list schemas)])
        (json-is? v schema)))]

    [{v (hash* ['oneOf (list schemas ...)]
               #:rest u)}
     (and
      (json-is? v u)
      (for/fold ([count 0]
                 #:result (= 1 count))
                ([schema (in-list schemas)]
                 #:break (< 1 count))
        (+ count (if (json-is? v schema) 1 0))))]
    
    [{v (hash* ['if cond]
               ['then then #:default Any]
               ['else else #:default Any]
               #:rest u)}
     (and
      (json-is? v u)
      (if (json-is? v cond)
          (json-is? v then)
          (json-is? v else)))]

    [{v (hash* ['type "null"]
               #:closed)}
     (eq? (json-null) v)]
    
    [{v (hash* ['type "boolean"]
               #:closed)}
     (boolean? v)]
    
    [{v (and schema
             (hash* ['type (list types ...)]
                    #:open))}
     (for/or ([type (in-list (remove-duplicates types))])
       (json-is? v (hash-set schema 'type type)))]
    
    [{(? number? v)
      (hash* ['type type #:default "number"]
             ['minimum minimum #:default -inf.0]
             ['maximum maximum #:default +inf.0]
             ['multipleOf multiple-of #:default #f]
             #:closed)}
     #:when (string=? "number" type)
     (and
      (<= minimum v maximum)
      (implies multiple-of
        (integer? (/ v multiple-of))))]

    [{(? integer? v)
      (hash* ['type type #:default "integer"]
             ['minimum minimum #:default -inf.0]
             ['maximum maximum #:default +inf.0]
             ['multipleOf multiple-of #:default #f]
             #:closed)}
     #:when (string=? "integer" type)
     (and
      (<= minimum v maximum)
      (implies multiple-of
        (integer? (/ v multiple-of))))]
    
    [{(? string? v)
      (hash* ['type type #:default "string"]
             ['enum options #:default #f]
             ['pattern rx #:default #f]
             ['minLength min-length #:default -inf.0]
             ['maxLength max-length #:default +inf.0]
             #:closed)}
     #:when (string=? "string" type)
     (and
      (<= min-length (string-length v) max-length)
      (implies options
        (member v options))
      (implies rx
        (regexp-match? (regexp rx) v)))]

    [{(? list? v)
      (hash* ['type type #:default "array"]
             ['items items]
             ['minItems min-items #:default 0]
             ['maxItems max-items #:default +inf.0]
             ['prefixItems prefix-items #:default #f]
             ['contains contains #:default #f]
             ['minContains min-contains #:default 1]
             ['maxContains max-contains #:default +inf.0]
             ['uniqueItems unique-items? #:default #f]
             #:closed)}
     #:when (string=? "array" type)
     (define n (length v))
     (and
      (<= min-items n max-items)
      (for/and ([item (in-list v)])
        (json-is? item items))
      (implies prefix-items
        (implies (not items)
          (= (length prefix-items) n))
        (for/and ([item (in-list v)]
                  [schema (in-list prefix-items)])
          (json-is? item schema)))
      (implies contains
        (<= min-contains
            (for/sum ([item (in-list v)])
              (if (json-is? item contains) 1 0))
            max-contains))
      (implies unique-items?
        (not (check-duplicates v))))]
    
    [{(? hash? v)
      (hash* ['type type #:default "object"]
             ['properties properties]
             ['required required #:default null]
             ['minProperties min-properties #:default 0]
             ['maxProperties max-properties #:default +inf.0]
             ['patternProperties pattern-properties #:default #f]
             ['additionalProperties additional-properties? #:default #t]
             ['dependentRequired dependent-required #:default #f]
             ['dependentSchemas dependent-schemas #:default #f]
             #:closed)}
     #:when (string=? "object" type)
     (define n (hash-count v))
     (and
      (<= min-properties n max-properties)
      (for/and ([key (in-list required)])
        (hash-has-key? v (string->symbol key)))
      (or
       additional-properties?
       (= (length required) n))
      (for/and ([(key v) (in-immutable-hash v)])
        (match (hash-ref properties key 'not-found)
          ['not-found
           (if (not pattern-properties)
               additional-properties?
               (let ([key (symbol->string key)])
                 (for/or ([(pattern schema) (in-immutable-hash pattern-properties)])
                   (and
                    (regexp-match? (regexp (symbol->string pattern)) key)
                    (json-is? v schema)))))]
          [schema
           (json-is? v schema)]))
      (implies dependent-required
        (for*/and ([(key deps) (in-immutable-hash dependent-required)]
                   #:when (hash-has-key? v key)
                   [dep (in-list deps)])
          (hash-has-key? v (string->symbol dep))))
      (implies dependent-schemas
        (for/and ([(key schema) (in-immutable-hash dependent-schemas)]
                  #:when (hash-has-key? v key))
          (json-is? v schema))))]
    
    [{_ _} #f]))