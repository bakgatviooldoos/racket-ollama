#lang racket/base

(require racket/hash
         threading)

(provide
  (all-defined-out))

(module schema-utils racket/base
  (require data/monocle
           json
           racket/list
           racket/match
           racket/set
           racket/symbol
           threading)
  
  (provide
    (all-defined-out))

  (define undefined (gensym))

  (define (undefined? x) (eq? undefined x))
  (define (json-null? x) (eq? (json-null) x))

  (define-syntax-rule
    (implies p q ...)
    (or (not p) (and q ...)))

  (define (combine-types k v1 v2)
    (case k
      [(type)
       (if (pair? v1)
           (cons v2 v1)
           (list v2 v1))]
      [else v2]))

  (define (kvs->hasheq kvs)
    (for/hasheq ([kv (in-list kvs)])
      (match-define (cons k v) kv)
      (values k v)))

  (define (kvs->keys kvs)
    (map (compose1 symbol->immutable-string car) kvs))

  ;; could be useful in client.rkt
  ;; instead of json-options (extend-client-endpoints)
  (define (json-&opt
           #:default [default undefined]
           #:nullable? [nullable? (json-null? default)]
           json &lens v [app values])
    (define u (if (undefined? v) default v))
    (cond
      [(undefined? u) json]
      [(and
        (not nullable?)
        (json-null? u))
       json]
      [else
       (&lens json (app u))]))
  
  (define &type (&opt-hash-ref 'type))
  
  (define &description (&opt-hash-ref 'description))

  ; conditional keywords
  (define &not (&opt-hash-ref 'not))
  (define &const (&opt-hash-ref 'const))
  
  (define &all-of (&opt-hash-ref 'allOf))
  (define &any-of (&opt-hash-ref 'anyOf))
  (define &one-of (&opt-hash-ref 'oneOf))
  
  (define &if (&opt-hash-ref 'if))
  (define &then (&opt-hash-ref 'then))
  (define &else (&opt-hash-ref 'else))

  ; string keywords
  (define &enum (&opt-hash-ref 'enum))
  (define &pattern (&opt-hash-ref 'pattern))
  (define &min-length (&opt-hash-ref 'minLength))
  (define &max-length (&opt-hash-ref 'maxLength))

  ; number keywords
  (define &minimum (&opt-hash-ref 'minimum))
  (define &maximum (&opt-hash-ref 'maximum))
  (define &multiple-of (&opt-hash-ref 'multipleOf))

  ; string | number keywords
  (define &format (&opt-hash-ref 'format))
  
  ; array keywords
  (define &items (&opt-hash-ref 'items))
  (define &min-items (&opt-hash-ref 'minItems))
  (define &max-items (&opt-hash-ref 'maxItems))
  (define &prefix-items (&opt-hash-ref 'prefixItems))
  (define &contains (&opt-hash-ref 'contains))
  (define &min-contains (&opt-hash-ref 'minContains))
  (define &max-contains (&opt-hash-ref 'maxContains))
  (define &unique-items? (&opt-hash-ref 'uniqueItems))
  (define &unevaluated-items (&opt-hash-ref 'unevaluatedItems))

  ; object keywords
  (define &properties (&opt-hash-ref 'properties))
  (define &required (&opt-hash-ref 'required))
  (define &min-properties (&opt-hash-ref 'minProperties))
  (define &max-properties (&opt-hash-ref 'maxProperties))
  (define &pattern-properties (&opt-hash-ref 'patternProperties))
  (define &dependent-required (&opt-hash-ref 'dependentRequired))
  (define &dependent-schemas (&opt-hash-ref 'dependentSchemas))
  (define &additional-properties (&opt-hash-ref 'additionalProperties))
  (define &unevaluated-properties (&opt-hash-ref 'unevaluatedProperties))

  (define (regexp->symbol rx) (string->symbol (object-name rx)))
  (define (symbol->regexp sym) (regexp (symbol->string sym)))
  (define (->pattern-properties pattern-props)
    (for/hasheq ([(rx schema) (in-immutable-hash pattern-props)])
      (values (regexp->symbol rx) schema)))

  (define (pattern-property-lookup pattern-props)
    (define patterns (map symbol->regexp (hash-keys pattern-props)))
    (define matches (hasheq))
    (lambda (prop)
      (cond
        [(hash-has-key? matches prop) (hash-ref matches prop)]
        [else
         (define key (symbol->string prop))
         (for/fold ([u undefined]
                    #:result
                    (begin0 u
                      (set! matches (hash-set matches prop u))))
                   ([rx (in-list patterns)]
                    #:break (not (undefined? u))
                    #:when (regexp-match? rx key))
           (hash-ref pattern-props (regexp->symbol rx)))])))

  ;; VALIDATION
  (define current-path (make-parameter #f))
  (define current-scope (make-parameter #f))
  
  (define empty-set (seteq))
  (define empty-hash (hasheq))
  
  (struct schema-error (scope schema path value reason) #:transparent)
  (struct validation-ctx (ok? annotations errors) #:transparent)

  (define ok (validation-ctx #t empty-set null))

  (define ((make-error value) schema reason)
    (schema-error
     (reverse (current-scope))
     schema
     (reverse (current-path))
     value
     reason))

  (define (invalidate-ctx ctx error)
    (~>> (validation-ctx-errors ctx)
         (cons error)
         (validation-ctx #f empty-set)))

  (define (prepare-ctx ok? ann err)
    (if ok?
        (validation-ctx ok? ann null)
        (validation-ctx #f empty-set err)))

  (define (ctx->ok?/ann/err ann err key)
    (match-lambda
      [(validation-ctx #f _ err*)
       (values #f ann (append err err*))]
      [_
       (values #t (set-add ann key) err)]))

  (define-syntax-rule
    (with-path path . body)
    (parameterize ([current-path (cons path (current-path))])
      . body))

  (define-syntax-rule
    (with-scope scope . body)
    (parameterize ([current-scope (cons scope (current-scope))])
      . body))

  (define (reason/tally-error msg less? count either suffix)
    (format
     msg
     (if less? (car either) (cdr either))
     count
     (if (= 1 count) (car suffix) (cdr suffix))))
  
  (define schema/check-literal
    (match-lambda**
      [{ctx _value (or #t (hash*))} ctx]
      [{ctx value #f}
       (define always-error (make-error value))
       (~>> "this schema always fails to match"
            (always-error #f)
            (invalidate-ctx ctx)
            (with-scope 'literal))]))

  (define schema/check-type
    (match-lambda**
      [{ctx _value _schema}
       #:when (not (validation-ctx-ok? ctx))
       ctx]
      
      [{ctx (? json-null?) (hash* ['type "null"])}    ctx]
      [{ctx (? boolean?)   (hash* ['type "boolean"])} ctx]
      [{ctx (? integer?)   (hash* ['type "integer"])} ctx]
      [{ctx (? real?)      (hash* ['type "number"])}  ctx]
      [{ctx (? string?)    (hash* ['type "string"])}  ctx]
      [{ctx (? list?)      (hash* ['type "array"])}   ctx]
      [{ctx (? hash?)      (hash* ['type "object"])}  ctx]
      
      [{ctx value (hash* ['type (list types ...)])}
       (for/fold ([ok? #f]
                  [err null]
                  #:result
                  (validation-ctx ok? empty-set (if ok? null err)))
                 
                 ([type (in-list types)]
                  #:break ok?)
         (~>> (hasheq 'type type)
              (schema/check-type ctx value)
              (match _
                [(validation-ctx #f _ err*)
                 (values ok? (append err err*))]
                [_
                 (values #t err)])))]
      
      [{ctx value (hash* ['type type])}
       (define type-error (make-error value))
       
       (define (reason/type-error)
         (~> "the value is not an instance of '~a'"
             (format type)))
       
       (~>> (reason/type-error)
            (type-error (hasheq 'type type))
            (invalidate-ctx ctx)
            (with-scope 'type))]
      
      [{ctx _value _schema} ctx]))

  (define schema/check-number
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? real? value)
        (hash* ['minimum minimum #:default -inf.0]
               ['maximum maximum #:default +inf.0]
               ['multipleOf multiple #:default #f])}

       (define number-error (make-error value))
       
       (define (reason/number-range-error less?)
         (~> "the number is ~a than ~a"
             (format
              (if less? "less" "greater")
              (if less? minimum maximum))))
       
       (define (number/check-range ctx)
         (cond
           [(<= minimum value maximum) ctx]
           [(< value minimum)
            (~>> (reason/number-range-error #;less? #t)
                 (number-error (hasheq 'minimum minimum))
                 (invalidate-ctx ctx)
                 (with-scope 'minimum))]
           [else
            (~>> (reason/number-range-error #;less? #f)
                 (number-error (hasheq 'maximum maximum))
                 (invalidate-ctx ctx)
                 (with-scope 'maximum))]))

       (define (reason/number-divisibility-error)
         (~> "the number is not a multiple of ~a"
             (format multiple)))
     
       (define (number/check-multiple ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(implies multiple (integer? (/ value multiple))) ctx]
           [else
            (~>> (reason/number-divisibility-error)
                 (number-error (hasheq 'multipleOf multiple))
                 (invalidate-ctx ctx)
                 (with-scope 'multipleOf))]))
     
       (~>> ctx
            (number/check-range)
            (number/check-multiple)
            (with-scope 'number))]

      [{ctx _value _schema} ctx]))

  (define schema/check-string
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? string? value)
        (hash* ['pattern rx #:default #f]
               ['enum options #:default undefined]
               ['minLength min-length #:default 0]
               ['maxLength max-length #:default +inf.0])}

       (define string-error (make-error value))
       
       (define length (string-length value))
       
       (define (reason/string-length-error less? which)
         (~> "the string has ~a than ~a character~a"
             (reason/tally-error less? which '("less" . "more") '("" . "s"))))
       
       (define (string/check-length ctx)
         (cond
           [(<= min-length length max-length) ctx]
           [(< length min-length)
            (~>> (reason/string-length-error #;less? #t min-length)
                 (string-error (hasheq 'minLength min-length))
                 (invalidate-ctx ctx)
                 (with-scope 'minLength))]
           [else
            (~>> (reason/string-length-error #;less? #f max-length)
                 (string-error (hasheq 'maxLength max-length))
                 (invalidate-ctx ctx)
                 (with-scope 'maxLength))]))

       (define (string/check-options ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? options) ctx]
           [(member value options) ctx]
           [else
            (~>> "the string is not a member of the enumeration"
                 (string-error (hasheq 'enum options))
                 (invalidate-ctx ctx)
                 (with-scope 'enum))]))

       (define (reason/string-pattern-error)
         (~> "the string does not match the regular-expression '~a'"
             (format rx)))

       (define (string/check-pattern ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(implies rx (regexp-match? (regexp rx) value)) ctx]
           [else
            (~>> (reason/string-pattern-error)
                 (string-error (hasheq 'pattern rx))
                 (invalidate-ctx ctx)
                 (with-scope 'pattern))]))
     
       (~>> ctx
            (string/check-length)
            (string/check-options)
            (string/check-pattern)
            (with-scope 'string))]
      
      [{ctx _value _schema} ctx]))

  (define schema/check-not
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        value
        (hash* ['not invalid])}
       (define ctx* (validate-schema value invalid))
       (cond
         [(not (validation-ctx-ok? ctx*)) ctx]
         [else
          (define contradiction-error (make-error value))
          (~>> "the value must never match this schema"
               (contradiction-error (hasheq 'not invalid))
               (invalidate-ctx ctx)
               (with-scope 'not))])]
      
      [{ctx _value _schema} ctx]))

  (define schema/check-const
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        value
        (hash* ['const const])}
       (cond
         [(equal? const value) ctx]
         [else
          (define const-error (make-error value))
          (~>> "the value is not equal to the constant expression"
               (const-error (hasheq 'const const))
               (invalidate-ctx ctx)
               (with-scope 'const))])]
      
      [{ctx _value _schema} ctx]))

  (define schema/check-if
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        value
        (hash* ['if test]
               ['then then #:default #t]
               ['else else #:default undefined])}
       (define ctx* (validate-ctx/schema ctx value test))
       (with-scope 'if
         (cond
           [(validation-ctx-ok? ctx*)
            (~>> (validate-ctx/schema ctx* value then)
                 (with-scope 'then))]
           [(undefined? else) ctx]
           [(~>> (validate-ctx/schema ctx value else)
                 (with-scope 'else))]))]
      
      [{ctx _value _schema} ctx]))
  
  (define schema/check-all-of
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        value
        (hash* ['allOf (list all ...)])}
       (with-scope 'allOf
         (for/fold ([ok? #t]
                    [ann (validation-ctx-annotations ctx)]
                    [err null]
                    #:result
                    (prepare-ctx ok? ann err))
                   
                   ([(schema index) (in-indexed (in-list all))]
                    #:break (not ok?))
           (~>> (validate-schema value schema)
                (with-scope index)
                (match _
                  [(validation-ctx #t ann* _)
                   (values ok? (set-union ann ann*) err)]
                  [(validation-ctx #f _ err*)
                   (values #f ann (append err err*))]))))]

      [{ctx _value _schema} ctx]))

  (define schema/check-any-of
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        value
        (hash* ['anyOf (list any ...)])}
       (with-scope 'anyOf
         (for/fold ([ok? #f]
                    [ann (validation-ctx-annotations ctx)]
                    [err null]
                    #:result
                    (prepare-ctx ok? ann err))
                   
                   ([(schema index) (in-indexed (in-list any))]
                    #:break ok?)
           (~>> (validate-schema value schema)
                (with-scope index)
                (match _
                  [(validation-ctx #t ann* _)
                   (values #t (set-union ann ann*) err)]
                  [(validation-ctx #f _ err*)
                   (values ok? ann (append err err*))]))))]
      
      [{ctx _value _schema} ctx]))

  (define schema/check-one-of
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        value
        (hash* ['oneOf (list one ...)])}
       (with-scope 'oneOf
         (for/fold ([count 0]
                    [ann (validation-ctx-annotations ctx)]
                    [err null]
                    #:result
                    (prepare-ctx (= count 1) ann err))
                   
                   ([(schema index) (in-indexed (in-list one))]
                    #:break (< 1 count))
           (~>> (validate-schema value schema)
                (with-scope index)
                (match _
                  [(validation-ctx #t ann* _)
                   (values (+ count 1) (set-union ann ann*) err)]
                  [(validation-ctx #f _ err*)
                   (values count ann (append err err*))]))))]
      
      [{ctx _value _schema} ctx]))

  (define schema/check-array
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? list? value)
        (hash* ['items items #:default undefined]
               ['minItems min-items #:default 0]
               ['maxItems max-items #:default +inf.0]
               ['prefixItems prefix-items #:default null]
               ['contains contains #:default undefined]
               ['minContains min-contains #:default 1]
               ['maxContains max-contains #:default +inf.0]
               ['uniqueItems unique-items? #:default #f]
               ['unevaluatedItems unevaluated-items #:default undefined])}

       (define array-error (make-error value))
       
       (define size (length value))
       (define prefix (length prefix-items))
       
       (define (reason/array-length-error less? which)
         (~> "the array has ~a than ~a item~a"
             (reason/tally-error less? which '("less" . "more") '("" . "s"))))

       (define (array/check-size ctx)
         (cond
           [(<= min-items size max-items) ctx]
           [(< size min-items)
            (~>> (reason/array-length-error #;less? #t min-items)
                 (array-error (hasheq 'minItems min-items))
                 (invalidate-ctx ctx)
                 (with-scope 'minItems))]
           [else
            (~>> (reason/array-length-error #;less? #f max-items)
                 (array-error (hasheq 'maxItems max-items))
                 (invalidate-ctx ctx)
                 (with-scope 'maxItems))]))
       
       (define (array/check-prefix-items ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(< size prefix)
            (~>> (reason/array-length-error #;less? #t prefix)
                 (array-error (hasheq 'prefixItems prefix-items))
                 (invalidate-ctx ctx)
                 (with-scope 'length))]
           [else
            (with-scope 'prefixItems
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([schema (in-list prefix-items)]
                         [(item index) (in-indexed (in-list value))]
                         #:break (not ok?))
                (~> (validate-member schema)
                    (_ ann err index item))))]))

       (define validate-items (validate-member items))
       (define (array/check-items ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? items) ctx]
           [else
            (with-scope 'items
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([item (in-list (drop value prefix))]
                         [index (in-naturals prefix)]
                         #:break (not ok?))
                (validate-items ann err index item)))]))
       
       (define (reason/array-content-error less? which)
         (~> "the array contains ~a than ~a instance~a of the schema"
             (reason/tally-error less? which '("less" . "more") '("" . "s"))))

       (define (array/check-contains ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? contains) ctx]
           [else
            (with-scope 'contains
              (for/fold ([count 0]
                         [ann (validation-ctx-annotations ctx)]
                         #:result
                         (cond
                           [(<= min-contains count max-contains)
                            (validation-ctx #t ann null)]
                           [(< count min-contains)
                            (~>> (reason/array-content-error #;less? #t min-contains)
                                 (array-error (hasheq 'contains contains
                                                      'minContains min-contains))
                                 (invalidate-ctx ctx)
                                 (with-scope 'minContains))]
                           [else
                            (~>> (reason/array-content-error #;less? #f max-contains)
                                 (array-error (hasheq 'contains contains
                                                      'maxContains max-contains))
                                 (invalidate-ctx ctx)
                                 (with-scope 'maxContains))]))
                        
                        ([(item index) (in-indexed (in-list value))]
                         #:break (< max-contains count))
                     (cond
                       [(~> (validate-schema item contains)
                            (validation-ctx-ok?))
                        (values (+ count 1) (set-add ann index))]
                       [else
                        (values count ann)])))]))
       
       (define (array/check-unique ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not unique-items?) ctx]
           [else
            (cond
              [(~> (check-duplicates value #:default undefined)
                   (undefined?))
               ctx]
              [else
               (~>> "the array contains at least one duplicate value"
                    (array-error (hasheq 'uniqueItems #t))
                    (invalidate-ctx ctx)
                    (with-scope 'uniqueItems))])]))

       (define validate-unevaluated-items (validate-member unevaluated-items))
       (define (array/check-unevaluated-items ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? unevaluated-items) ctx]
           [else
            (with-scope 'unevaluatedItems
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([(item index) (in-indexed (in-list value))]
                         #:break (not ok?)
                         #:unless (set-member? ann index))
                (validate-unevaluated-items ann err index item)))]))
       
       (~>> ctx
            (array/check-size)
            (array/check-prefix-items)
            (array/check-items)
            (array/check-contains)
            (array/check-unique)
            (array/check-unevaluated-items)
            (with-scope 'array))]

      [{ctx _value _schema} ctx]))
  
  (define schema/check-object
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? hash? value)
       (hash* ['properties props #:default empty-hash]
              ['required required #:default null]
              ['minProperties min-props #:default 0]
              ['maxProperties max-props #:default +inf.0]
              ['patternProperties pattern-props #:default empty-hash]
              ['dependentRequired dependent-required #:default #f]
              ['dependentSchemas dependent-schemas #:default #f]
              ['additionalProperties additional-props #:default undefined]
              ['unevaluatedProperties unevaluated-props #:default undefined])}

       (define object-error (make-error value))
       
       (define size (hash-count value))
       (define keys (hash-keys value))

       (define (reason/object-size-error less? which)
         (~> "the object has ~a than ~a propert~a"
             (reason/tally-error less? which '("less" . "more") '("y" . "ies"))))
       
       (define (object/check-size ctx)
         (cond
           [(<= min-props size max-props) ctx]
           [(< size min-props)
            (~>> (reason/object-size-error #;less? #t min-props)
                 (object-error (hasheq 'minProperties min-props))
                 (invalidate-ctx ctx)
                 (with-scope 'minProperties))]
           [else
            (~>> (reason/object-size-error #;less? #f max-props)
                 (object-error (hasheq 'maxProperties max-props))
                 (invalidate-ctx ctx)
                 (with-scope 'maxProperties))]))
       
       (define (has-all-properties? required)
         (subset? (map string->symbol required) keys))
       
       (define (object/check-required ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(has-all-properties? required) ctx]
           [else
            (~>> "the object is missing at least one required property"
                 (object-error (hasheq 'required required))
                 (invalidate-ctx ctx)
                 (with-scope 'required))]))

       (define (object/check-dependent-required ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not dependent-required) ctx]
           [else
            (for/fold ([ctx ctx]
                       #:result ctx)
                      ([(prop req) (in-immutable-hash dependent-required)]
                       #:break (not (validation-ctx-ok? ctx))
                       #:when (hash-has-key? value prop))
              (cond
                [(has-all-properties? req) ctx]
                [else
                 (~>> "the object is missing at least one required property"
                      (object-error (hasheq 'dependentRequired (hasheq prop req)))
                      (invalidate-ctx ctx)
                      (with-scope 'dependentRequired))]))]))
       
       (define (object/check-dependent-schemas ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not dependent-schemas) ctx]
           [else
            (define ann (validation-ctx-annotations ctx))
            (with-scope 'dependentSchemas
              (for/fold ([ok? #t]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([(prop schema) (in-immutable-hash dependent-schemas)]
                         #:break (not ok?)
                         #:when (hash-has-key? value prop))
                (~> (validate-schema value schema)
                    (match
                      [(validation-ctx #f _ err*)
                       (values #f (append err err*))]
                      [_
                       (values ok? err)]))))]))
       
       (define (object/check-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [else
            (with-scope 'properties
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))

                        ([(prop schema) (in-immutable-hash props)]
                         #:break (not ok?)
                         #:when (hash-has-key? value prop))
                (~> (validate-member schema)
                    (_ ann err prop (hash-ref value prop)))))]))
       
       (define rx-lookup (pattern-property-lookup pattern-props))
       (define (object/check-pattern-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [else
            (with-scope 'patternProperties
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([(prop u) (in-immutable-hash value)]
                         #:break (not ok?))
                (define schema (rx-lookup prop))
                (cond
                  [(undefined? schema) (values ok? ann err)]
                  [else
                   (~> (validate-member schema)
                       (_ ann err prop u))])))]))

       (define validate-additional-props (validate-member additional-props))
       (define (object/check-additional-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? additional-props) ctx]
           [else
            (with-scope 'additionalProperties
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([(prop u) (in-immutable-hash value)]
                         #:break (not ok?)
                         #:when (and (not (hash-has-key? props prop))
                                     (undefined? (rx-lookup prop))))
                (validate-additional-props ann err prop u)))]))

       (define validate-unevaluated-props (validate-member unevaluated-props))
       (define (object/check-unevaluated-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? unevaluated-props) ctx]
           [else
            (with-scope 'unevaluatedProperties
              (for/fold ([ok? #t]
                         [ann (validation-ctx-annotations ctx)]
                         [err null]
                         #:result
                         (prepare-ctx ok? ann err))
                        
                        ([(prop u) (in-immutable-hash value)]
                         #:break (not ok?)
                         #:unless (set-member? ann prop))
                (validate-unevaluated-props ann err prop u)))]))
       
       (~>> ctx
            (object/check-size)
            (object/check-required)
            (object/check-dependent-required)
            (object/check-dependent-schemas)
            (object/check-properties)
            (object/check-pattern-properties)
            (object/check-additional-properties)
            (object/check-unevaluated-properties)
            (with-scope 'object))]
      
      [{ctx _value _schema} ctx]))

  (define (validate-ctx/schema ctx value schema)
    (~> ctx
        ; check simple constraints
        (schema/check-literal value schema)
        (schema/check-type    value schema)
        (schema/check-number  value schema)
        (schema/check-string  value schema)
        (schema/check-not     value schema)
        (schema/check-const   value schema)
        ; collect/propagate annotations
        (schema/check-if      value schema)
        (schema/check-all-of  value schema)
        (schema/check-any-of  value schema)
        (schema/check-one-of  value schema)
        ; use propagated annotations
        (schema/check-array   value schema)
        (schema/check-object  value schema)))

  (define ((validate-member schema) ann err key value)
    (~>> (validate-schema value schema)
         (with-path key)
         ((ctx->ok?/ann/err ann err key))))

  (define (validate-schema value schema)
    (validate-ctx/schema ok value schema)))

(require 'schema-utils)

(define None    #f)
(define Some    empty-hash)
(define Null    (&type Some "null"))
(define Boolean (&type Some "boolean"))
(define Number  (&type Some "number"))
(define Integer (&type Some "integer"))
(define String  (&type Some "string"))

(define (Array schema)
  (~> Some
      (&type "array")
      (&items schema)))

(define (Enum . options)
  (~> Some
      (&type "string")
      (&enum options)))

(define (Object* kvs)
  (~> Some
      (&type "object")
      (&required (kvs->keys kvs))
      (&properties (kvs->hasheq kvs))))

(define (Or . schemas)
  (apply hash-union schemas #:combine/key combine-types))

(define (Optional schema)
  (Or Null schema))

(define (Just value) (&const Some value))

(define (Not #:in [schema Some] invalid) (&not schema invalid))
(define (AllOf #:in [schema Some] . all) (&all-of schema all))
(define (AnyOf #:in [schema Some] . any) (&any-of schema any))
(define (OneOf #:in [schema Some] . one) (&one-of schema one))

(define (If cond
            #:in [schema Some]
            #:then [then #t]
            #:else [else undefined])
  (~> schema
      (json-&opt &if cond)
      (json-&opt &then then)
      (json-&opt &else else)))

(define (with-description schema description)
  (&description schema description))

(define (with-String [schema Some]
          #:pattern [rx undefined]
          #:min-length [min-length undefined]
          #:max-length [max-length undefined]
          #:format [format undefined])
  (~> schema
      (json-&opt &pattern rx object-name)
      (json-&opt &min-length min-length)
      (json-&opt &max-length max-length)
      (json-&opt &format format)))

(define (with-Number [schema Some]
          #:minimum [minimum undefined]
          #:maximum [maximum undefined]
          #:multiple-of [multiple-of undefined]
          #:format [format undefined])
  (~> schema
      (json-&opt &minimum minimum)
      (json-&opt &maximum maximum)
      (json-&opt &multiple-of multiple-of)
      (json-&opt &format format)))

(define (with-Array [schema Some]
          #:items [items undefined]
          #:min-items [min-items undefined]
          #:max-items [max-items undefined]
          #:prefix-items [prefix-items undefined]
          #:contains [contains undefined]
          #:min-contains [min-contains undefined]
          #:max-contains [max-contains undefined]
          #:unique-items? [unique-items? undefined]
          #:unevaluated-items [unevaluated-items undefined])
  (~> schema
      (json-&opt &items items)
      (json-&opt &min-items min-items)
      (json-&opt &max-items max-items)
      (json-&opt &prefix-items prefix-items)
      (json-&opt &contains contains)
      (json-&opt &min-contains min-contains)
      (json-&opt &max-contains max-contains)
      (json-&opt &unique-items? unique-items?)
      (json-&opt &unevaluated-items unevaluated-items)))

(define (with-Object [schema Some]
          #:properties [props undefined]
          #:required [required undefined]
          #:min-properties [min-props undefined]
          #:max-properties [max-props undefined]
          #:pattern-properties [pattern-props undefined]
          #:dependent-required [dependent-required undefined]
          #:dependent-schemas [dependent-schemas undefined]
          #:additional-properties [additional-props undefined]
          #:unevaluated-properties [unevaluated-props undefined])
  (~> schema
      (json-&opt &properties props)
      (json-&opt &required required)
      (json-&opt &min-properties min-props)
      (json-&opt &max-properties max-props)
      (json-&opt &pattern-properties pattern-props ->pattern-properties)
      (json-&opt &dependent-required dependent-required)
      (json-&opt &dependent-schemas dependent-schemas)
      (json-&opt &additional-properties additional-props)
      (json-&opt &unevaluated-properties unevaluated-props)))

(define (json/schema-errors? value schema)
  (define ctx (parameterize ([current-scope null]
                             [current-path null])
                (validate-schema value schema)))
  (~> (validation-ctx-ok? ctx)
      (if #f (validation-ctx-errors ctx))))

(define schema₁
  #hasheq((type . "object")
          (if . #hasheq((properties . #hasheq((type . #hasheq((const . "business")))))
                        (required . ("type"))
                        (type . "object")))
          (then . #hasheq((properties . #hasheq((department . #hasheq((type . "string")))))))
          (properties . #hasheq((city . #hasheq((type . "string")))
                                (state . #hasheq((type . "string")))
                                (street_address . #hasheq((type . "string")))
                                (type . #hasheq((enum . ("residential" "business"))))))
          (required . ("street_address" "city" "state" "type"))
          (unevaluatedProperties . #f)))

(define data₁
  #hasheq((city . "Washington")
          (department . "HR")
          (state . "DC")
          (street_address . "1600 Pennsylvania Avenue NW")
          (type . "business")))

(define data₂
  #hasheq((city . "Washington")
          (department . "HR")
          (state . "DC")
          (street_address . "1600 Pennsylvania Avenue NW")
          (type . "residential")))

(json/schema-errors? data₁ schema₁)
(json/schema-errors? data₂ schema₁)

(define schema₂
  #hasheq((prefixItems . (#hasheq((type . "string"))
                          #hasheq((type . "number"))))
          (unevaluatedItems . #f)))

(json/schema-errors? '("foo" 42) schema₂)
(json/schema-errors? '("foo" 42 null) schema₂)

(json/schema-errors?
 '(2 "2" 3 (1))
 (with-Array Some
   #:prefix-items
   (list
    Number
    (Or Number String)
    (Or Number String)
    (with-Array (Array Number)
      #:min-items 2))
   #:contains Number
   #:min-contains 2
   #:max-contains 3))

(json/schema-errors? #f (AnyOf String Number))

(json/schema-errors?
 (hasheq 'hello "hello")
 (Object* `([hello . ,(Just "world")])))

(json/schema-errors?
 "this string is too long"
 (with-String Some
   #:max-length 7))