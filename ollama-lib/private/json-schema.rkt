#lang racket/base

(require json
         racket/hash
         racket/list
         racket/match
         racket/symbol)

(provide
  (all-defined-out))

(define None    #f)
(define Some    (hasheq))
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
(define (Just value) (hasheq 'const value))

(define (AllOf #:in [in Some] . types) (hash-set in 'allOf types))
(define (AnyOf #:in [in Some] . types) (hash-set in 'anyOf types))
(define (OneOf #:in [in Some] . types) (hash-set in 'oneOf types))

(define (Condition #:in [in Some] #:if cond #:then then #:else [else #f])
  (hash-set* in 'if cond 'then then 'else else))

(define (with-description type description)
  (hash-set type 'description description))

(module schema-utils racket/base
  (require json)
  (provide
    (all-defined-out))

  (define _ (gensym))
  (define ∃ jsexpr?)

  (define current-unevaluated-items (make-parameter #t))
  (define current-unevaluated-properties (make-parameter #t))
  
  (define-syntax-rule
    (depends [e p] [k v])
    (if (not (∃ p)) e (hash-set e k v)))

  (define-syntax-rule
    (implies p q ...)
    (or (not p) (and q ...))))

(require 'schema-utils)

(define (with-String [type Some]
          #:pattern [rx _]
          #:min-length [min-length _]
          #:max-length [max-length _]
          #:format [format _])
  (let* ([type (depends [type rx] ['pattern (object-name rx)])]
         [type (depends [type min-length] ['minLength min-length])]
         [type (depends [type max-length] ['maxLength max-length])]
         [type (depends [type format] ['format (symbol->string format)])])
    type))

(define (with-Number [type Some]
          #:minimum [minimum _]
          #:maximum [maximum _]
          #:multiple-of [multiple-of _]
          #:format [format _])
  (let* ([type (depends [type minimum] ['minimum minimum])]
         [type (depends [type maximum] ['maximum maximum])]
         [type (depends [type multiple-of] ['multipleOf multiple-of])]
         [type (depends [type format] ['format (symbol->string format)])])
    type))

(define (with-Array [type Some]
          #:items [items _]
          #:min-items [min-items _]
          #:max-items [max-items _]
          #:prefix-items [prefix-items _]
          #:contains [contains _]
          #:min-contains [min-contains _]
          #:max-contains [max-contains _]
          #:unique-items? [unique-items? _]
          #:unevaluated-items [unevaluated-items _])
  (let* ([type (depends [type items] ['items items])]
         [type (depends [type min-items] ['minItems min-items])]
         [type (depends [type max-items] ['maxItems max-items])]
         [type (depends [type prefix-items] ['prefixItems prefix-items])]
         [type (depends [type contains] ['contains contains])]
         [type (depends [type min-contains] ['minContains min-contains])]
         [type (depends [type max-contains] ['maxContains max-contains])]
         [type (depends [type unique-items?] ['uniqueItems unique-items?])]
         [type (depends [type unevaluated-items] ['unevaluatedItems unevaluated-items])])
    type))

(define (with-Object [type Some]
          #:properties [props _]
          #:required [required _]
          #:min-properties [min-props _]
          #:max-properties [max-props _]
          #:pattern-properties [pattern-props _]
          #:dependent-required [dependent-required _]
          #:dependent-schemas [dependent-schemas _]
          #:additional-properties [additional-props _]
          #:unevaluated-properties [unevaluated-props _])
  (let* ([type (depends [type props] ['props props])]
         [type (depends [type required] ['required required])]
         [type (depends [type min-props] ['minProperties min-props])]
         [type (depends [type max-props] ['maxProperties max-props])]
         [type (depends [type dependent-required] ['dependentRequired dependent-required])]
         [type (depends [type dependent-schemas] ['dependentSchemas dependent-schemas])]
         [type (depends [type additional-props] ['additionalProperties additional-props])]
         [type (depends [type unevaluated-props] ['unevaluatedProperties unevaluated-props])]
         [type (depends [type pattern-props]
                 ['patternProperties
                  (for/hasheq ([(rx schema) (in-immutable-hash pattern-props)])
                    (values (string->symbol (object-name rx)) schema))])])
    type))

(define json-is?
  (match-lambda**
    [{_ #f} #f]
    [{_ #t} #t]
    
    [{_ (hash #:closed)} #t]
    
    [{v (hash* ['not schema]
               #:closed)}
     (not (json-is? v schema))]
    
    [{v (hash* ['const value]
               #:closed)}
     (equal? value v)]
    
    [{v (hash* ['allOf (list schemas ...)]
               ['unevaluatedItems unevaluated-items #:default #t]
               ['unevaluatedProperties unevaluated-properties #:default #t]
               #:rest u)}
     (parameterize ([current-unevaluated-items unevaluated-items]
                    [current-unevaluated-properties unevaluated-properties])
       (and
        (json-is? v u)
        (for/and ([schema (in-list schemas)])
          (json-is? v schema))))]

    [{v (hash* ['anyOf (list schemas ...)]
               ['unevaluatedItems unevaluated-items #:default #t]
               ['unevaluatedProperties unevaluated-properties #:default #t]
               #:rest u)}
     (parameterize ([current-unevaluated-items unevaluated-items]
                    [current-unevaluated-properties unevaluated-properties])
       (and
        (json-is? v u)
        (for/or ([schema (in-list schemas)])
          (json-is? v schema))))]

    [{v (hash* ['oneOf (list schemas ...)]
               ['unevaluatedItems unevaluated-items #:default #t]
               ['unevaluatedProperties unevaluated-properties #:default #t]
               #:rest u)}
     (parameterize ([current-unevaluated-items unevaluated-items]
                    [current-unevaluated-properties unevaluated-properties])
       (and
        (json-is? v u)
        (for/fold ([count 0]
                   #:result (= 1 count))
                  ([schema (in-list schemas)]
                   #:break (< 1 count))
          (+ count (if (json-is? v schema) 1 0)))))]

    [{v (and (hash* ['type (list types ...)]
                    #:open)
             schema)}
     (for/or ([type (in-list (remove-duplicates types))])
       (json-is? v (hash-set schema 'type type)))]
    
    [{v (hash* ['if cond]
               ['then then]
               ['else else #:default #f]
               #:rest u)}
     (and
      (json-is? v u)
      (if (json-is? v cond)
          (json-is? v then)
          (json-is? v else)))]

    [{v (hash* ['type "null"]
               #:open)}
     (eq? (json-null) v)]
    
    [{v (hash* ['type "boolean"]
               #:open)}
     (boolean? v)]
    
    [{(? integer? v)
      (hash* ['type type #:default #f]
             ['minimum minimum #:default #f]
             ['maximum maximum #:default #f]
             ['multipleOf multiple-of #:default #f]
             #:open)}
     #:when (or (and type (string=? "integer" type))
                minimum maximum multiple-of)
     (let ([minimum (or minimum -inf.0)]
           [maximum (or maximum +inf.0)])
       (and
        (<= minimum v maximum)
        (implies multiple-of
          (integer? (/ v multiple-of)))))]

    [{(? number? v)
      (hash* ['type type #:default #f]
             ['minimum minimum #:default #f]
             ['maximum maximum #:default #f]
             ['multipleOf multiple-of #:default #f]
             #:open)}
     #:when (or (and type (string=? "number" type))
                minimum maximum multiple-of)
     (let ([minimum (or minimum -inf.0)]
           [maximum (or maximum +inf.0)])
       (and
        (<= minimum v maximum)
        (implies multiple-of
          (integer? (/ v multiple-of)))))]
    
    [{(? string? v)
      (hash* ['type type #:default #f]
             ['enum options #:default #f]
             ['pattern rx #:default #f]
             ['minLength min-length #:default #f]
             ['maxLength max-length #:default #f]
             #:open)}
     #:when (or (and type (string=? "string" type))
                options rx min-length max-length)
     (let ([min-length 0]
           [max-length +inf.0])
       (and
        (<= min-length (string-length v) max-length)
        (implies options
          (memv v options))
        (implies rx
          (regexp-match? (regexp rx) v))))]

    [{(? list? v)
      (hash* ['type type #:default "array"]
             ['items items]
             ['minItems min-items #:default 0]
             ['maxItems max-items #:default +inf.0]
             ['prefixItems prefix-items #:default null]
             ['contains contains #:default _]
             ['minContains min-contains #:default 1]
             ['maxContains max-contains #:default +inf.0]
             ['uniqueItems unique-items? #:default #f]
             ['unevaluatedItems unevaluated-items #:default (current-unevaluated-items)]
             #:open)}
     #:when (string=? "array" type)
     (define n (length v))
     (define m (length prefix-items))
     (parameterize ([current-unevaluated-items #t])
       (and
        (<= min-items n max-items)
        (implies (< 0 m)
          (implies (not items) (= m n))
          (for/and ([item (in-list v)]
                    [schema (in-list prefix-items)])
            (json-is? item schema)))
        (for/and ([item (in-list (drop v m))])
          (or (json-is? item items)
              (json-is? item unevaluated-items)))
        (implies (∃ contains)
          (for/fold ([count 0]
                     #:result
                     (<= min-contains
                         count
                         max-contains))
                    ([item (in-list v)]
                     #:break (< max-contains count)
                     #:when (json-is? item contains))
            (+ count 1)))
        (implies unique-items?
          (not (check-duplicates v)))))]
    
    [{(? hash? v)
      (hash* ['type type #:default "object"]
             ['properties props]
             ['required required #:default null]
             ['minProperties min-props #:default 0]
             ['maxProperties max-props #:default +inf.0]
             ['patternProperties pattern-props #:default #f]
             ['additionalProperties additional-props #:default #t]
             ['dependentRequired dependent-required #:default #f]
             ['dependentSchemas dependent-schemas #:default #f]
             ['unevaluatedProperties unevaluated-props #:default (current-unevaluated-properties)]
             #:open)}
     #:when (string=? "object" type)
     (define n (hash-count v))
     (parameterize ([current-unevaluated-properties #t])
       (and
        (<= min-props n max-props)
        (for/and ([key (in-list required)])
          (hash-has-key? v (string->symbol key)))
        (for/and ([(key v) (in-immutable-hash v)])
          (match (hash-ref props key 'not-found)
            ['not-found
             (or (let ([key (symbol->string key)])
                   (and pattern-props
                        (for/or ([(pattern schema) (in-immutable-hash pattern-props)]
                                 #:when (regexp-match? (regexp (symbol->string pattern)) key))
                          (json-is? v schema))))
                 (json-is? v additional-props)
                 (json-is? v unevaluated-props))]
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
            (json-is? v schema)))))]
    
    [{_ _} #f]))

(define a-schema
  '#hasheq((if . #hasheq((properties . #hasheq((type . #hasheq((const . "business")))))
                         (required . ("type"))
                         (type . "object")))
           (properties
            .
            #hasheq((city . #hasheq((type . "string")))
                    (state . #hasheq((type . "string")))
                    (street_address . #hasheq((type . "string")))
                    (type . #hasheq((enum . ("residential" "business"))))))
           (required . ("street_address" "city" "state" "type"))
           (then . #hasheq((properties . #hasheq((department . #hasheq((type . "string")))))))
           (type . "object")
           (unevaluatedProperties . #f)))

(define data₁
  '#hasheq((city . "Washington")
           (department . "HR")
           (state . "DC")
           (street_address . "1600 Pennsylvania Avenue NW")
           (type . "business")))

(define data₂
  '#hasheq((city . "Washington")
           (department . "HR")
           (state . "DC")
           (street_address . "1600 Pennsylvania Avenue NW")
           (type . "residential")))

(json-is? data₁ a-schema) ;; #true
(json-is? data₂ a-schema) ;; #false