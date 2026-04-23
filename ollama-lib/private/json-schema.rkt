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
           #:null? [nullable? (json-null? default)]
           json &key v [app values])
    (let ([v (if (undefined? v) default v)])
      (cond
        [(undefined? v) json]
        [(and
          (not nullable?)
          (json-null? v))
         json]
        [else
         (&key json (app v))])))
  
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

  (define current-path (make-parameter #f))
  
  (define empty-set (set))

  ;; VALIDATION
  (struct schema-error (path schema value reason) #:transparent)
  (struct validation-ctx (ok? annotations errors) #:transparent)

  (define ok (validation-ctx #t empty-set null))

  (define (invalidate-ctx ctx schema v reason)
    (~> (reverse (current-path))
        (schema-error schema v reason)
        (cons (validation-ctx-errors ctx))
        (validation-ctx #f empty-set _)))

  (define schema/check-literal
    (match-lambda**
      [{ctx _v (or #t (hash*))} ctx]
      [{ctx v #f}
       (~> (list #f v "no value can match this schema")
           (apply invalidate-ctx ctx _))]))

  (define (reason/type-error value type)
    (~> "the value ~a is not an instance of type '~a'"
        (format (jsexpr->string value) type)))

  (define schema/check-type
    (match-lambda**
      [{ctx _v _schema}
       #:when (not (validation-ctx-ok? ctx))
       ctx]
    
      [{ctx (? json-null?) (hash* ['type "null"])}    ctx]
      [{ctx (? boolean?)   (hash* ['type "boolean"])} ctx]
      [{ctx (? integer?)   (hash* ['type "integer"])} ctx]
      [{ctx (? real?)      (hash* ['type "number"])}  ctx]
      [{ctx (? string?)    (hash* ['type "string"])}  ctx]
      [{ctx (? list?)      (hash* ['type "array"])}   ctx]
      [{ctx (? hash?)      (hash* ['type "object"])}  ctx]

      [{ctx v (hash* ['type (list types ...)])}
       (for/fold ([ok? #f]
                  [err null]
                  #:result
                  (validation-ctx ok? empty-set (if ok? null err)))
                 ([type (in-list types)]
                  #:break ok?)
         (define ctx* (schema/check-type ctx v (hasheq 'type type)))
         (values
          (or ok? (validation-ctx-ok? ctx*))
          (append err (validation-ctx-errors ctx*))))]
    
      [{ctx v (hash* ['type type])}
       (~> (hasheq 'type type)
           (list v (reason/type-error v type))
           (apply invalidate-ctx ctx _))]
    
      [{ctx _v _schema} ctx]))

  (define (reason/const-error value const)
    (~> "the value ~a is not equal to the constant ~a"
        (format
         (jsexpr->string value)
         (jsexpr->string const))))

  (define schema/check-const
    (match-lambda**
      [{(? validation-ctx-ok? ctx) v
        (hash* ['const const])}
       (cond
         [(equal? const v) ctx]
         [else
          (~> (hasheq 'const const)
              (list v (reason/const-error v const))
              (apply invalidate-ctx ctx _))])]

      [{ctx _v _schema} ctx]))

  (define (reason/number-range-error value minimum maximum)
    (define less? (< value minimum))
    (~> "the number ~a is ~a than ~a"
        (format
         value
         (if less? "less" "greater")
         (if less? minimum maximum))))

  (define (reason/number-multiplicity-error value multiple)
    (~> "the number ~a is not a multiple of ~a"
        (format value multiple)))

  (define schema/check-number
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? real? v)
        (hash* ['minimum minimum #:default -inf.0]
               ['maximum maximum #:default +inf.0]
               ['multipleOf multiple #:default #f])}
       (define (number/check-range ctx)
         (cond
           [(<= minimum v maximum) ctx]
           [else
            (~> (hasheq 'minimum minimum
                        'maximum maximum)
                (list v (reason/number-range-error v minimum maximum))
                (apply invalidate-ctx ctx _))]))
     
       (define (number/check-multiple ctx)
         (cond
           [(implies multiple (integer? (/ v multiple))) ctx]
           [else
            (~> (hasheq 'multipleOf multiple)
                (list v (reason/number-multiplicity-error v multiple))
                (apply invalidate-ctx ctx _))]))
     
       (~> ctx
           (number/check-range)
           (number/check-multiple))]

      [{ctx _v _schema} ctx]))

  (define (reason/string-length-error value n min-length max-length)
    (define less? (< n min-length))
    (~> "the string ~a has ~a than ~a characters"
        (format
         (jsexpr->string value)
         (if less? "less" "more")
         (if less? min-length max-length))))

  (define (reason/string-pattern-error value rx)
    (~> "the string ~a does not match the regular-expression '~a'"
        (format (jsexpr->string value) rx)))

  (define schema/check-string
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? string? v)
        (hash* ['pattern rx #:default #f]
               ['minLength min-length #:default 0]
               ['maxLength max-length #:default +inf.0])}
       (define n (string-length v))
     
       (define (string/check-length ctx)
         (cond
           [(<= min-length n max-length) ctx]
           [else
            (~> (hasheq 'minLength min-length
                        'maxLength max-length)
                (list v (reason/string-length-error v n min-length max-length))
                (apply invalidate-ctx ctx _))]))

       (define (string/check-pattern ctx)
         (cond
           [(implies rx (regexp-match? (regexp rx) v)) ctx]
           [else
            (~> (hasheq 'pattern rx)
                (list v (reason/string-pattern-error v rx))
                (apply invalidate-ctx ctx _))]))
     
       (~> ctx
           (string/check-length)
           (string/check-pattern))]
    
      [{ctx _v _schema} ctx]))

  (define (reason/array-size-error value n min-items max-items)
    (define less? (< n min-items))
    (define which (if less? min-items max-items))
    (~> "the array ~a has ~a than ~a item~a"
        (format
         (jsexpr->string value)
         (if less? "less" "more")
         which
         (if (= 1 which) "" "s"))))

  (define (reason/array-content-error value contains n min-contains max-contains)
    (define less? (< n min-contains))
    (define which (if less? min-contains max-contains))
    (~> "the array ~a contains ~a than ~a instance~a of ~a"
        (format
         (jsexpr->string value)
         (if less? "less" "more")
         which
         (if (= 1 which) "" "s")
         (jsexpr->string contains))))

  (define (reason/array-duplicate-item-error value duplicate)
    (~> "the array ~a contains at least one duplicate of ~a"
        (format
         (jsexpr->string value)
         (jsexpr->string duplicate))))

  (define schema/check-array
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? list? v)
        (hash* ['items items #:default undefined]
               ['minItems min-items #:default 0]
               ['maxItems max-items #:default +inf.0]
               ['prefixItems prefix-items #:default null]
               ['contains contains #:default undefined]
               ['minContains min-contains #:default 1]
               ['maxContains max-contains #:default +inf.0]
               ['uniqueItems unique-items? #:default #f]
               ['unevaluatedItems unevaluated-items #:default undefined])}
       (define n (length v))
       (define m (length prefix-items))

       (define (array/check-size ctx)
         (cond
           [(<= min-items n max-items) ctx]
           [else
            (~> (hasheq 'minItems min-items
                        'maxItems max-items)
                (list v (reason/array-size-error v n min-items max-items))
                (apply invalidate-ctx ctx _))]))
     
       (define (array/check-prefix-items ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(< n m)
            (~> (hasheq 'prefixItems prefix-items)
                (list v (reason/array-size-error v n m +inf.0))
                (apply invalidate-ctx ctx _))]
           [else
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result
                       (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([schema (in-list prefix-items)]
                       [(item index) (in-indexed (in-list v))]
                       #:break (not ok?))
              (define ctx* (parameterize ([current-path (cons index (current-path))])
                             (validate/schema ok item schema)))
              (cond
                [(validation-ctx-ok? ctx*)
                 (values ok? (set-add ann index) err)]
                [else
                 (values #f ann (append err (validation-ctx-errors ctx*)))]))]))

       (define (array/check-items ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? items) ctx]
           [else
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([item (in-list (drop v m))]
                       [index (in-naturals m)]
                       #:break (not ok?))
              (define ctx* (parameterize ([current-path (cons index (current-path))])
                             (validate/schema ok item items)))
              (cond
                [(validation-ctx-ok? ctx*)
                 (values ok? (set-add ann index) err)]
                [else
                 (values #f ann (append err (validation-ctx-errors ctx*)))]))]))

       (define (array/check-contains ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? contains) ctx]
           [else
            (for/fold ([ann (validation-ctx-annotations ctx)]
                       [count 0]
                       #:result
                       (cond
                         [(<= min-contains count max-contains)
                          (validation-ctx #t ann null)]
                         [else
                          (~> (hasheq 'contains contains
                                      'minContains min-contains
                                      'maxContains max-contains)
                              (list v (reason/array-content-error
                                       v contains count min-contains max-contains))
                              (apply invalidate-ctx ctx _))]))
                      
                      ([(item index) (in-indexed (in-list v))]
                       #:break (< max-contains count))

              (define ctx* (parameterize ([current-path (cons index (current-path))])
                             (validate/schema ok item contains)))
              (cond
                [(not (validation-ctx-ok? ctx*))
                 (values ann count)]
                [else
                 (values (set-add ann index) (+ count 1))]))]))

       (define (array/check-unique ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not unique-items?) ctx]
           [else
            (define dup (check-duplicates v #:default undefined))
            (if (undefined? dup)
                ctx
                (~> (hasheq 'uniqueItems #t)
                    (list v (reason/array-duplicate-item-error v dup))
                    (apply invalidate-ctx ctx _)))]))

       (define (array/check-unevaluated-items ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not unevaluated-items) ctx]
           [else
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result
                       (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([(item index) (in-indexed (in-list v))]
                       #:break (not ok?)
                       #:unless (set-member? ann index))
              (define ctx* (parameterize ([current-path (cons index (current-path))])
                             (validate/schema ok item unevaluated-items)))
              (cond
                [(validation-ctx-ok? ctx*) (values ok? (set-add ann index) err)]
                [else
                 (values #f ann (append err (validation-ctx-errors ctx*)))]))]))
       
       (~> ctx
           (array/check-size)
           (array/check-prefix-items)
           (array/check-items)
           (array/check-contains)
           (array/check-unique)
           (array/check-unevaluated-items))]

      [{ctx _v _schema} ctx]))

  (define (reason/object-size-error value n min-props max-props)
    (define less? (< n min-props))
    (define which (if less? min-props max-props))
    (~> "the object ~a has ~a than ~a propert~a"
        (format
         (jsexpr->string value)
         (if less? "less" "more")
         which
         (if (= 1 which) "y" "ies"))))

  (define (reason/object-required-properties-error value required)
    (~> "the object ~a is missing at least one required property in ~a"
        (format
         (jsexpr->string value)
         (jsexpr->string required))))
  
  (define schema/check-object
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        (? hash? v)
       (hash* ['properties props #:default #f]
              ['required required #:default null]
              ['minProperties min-props #:default 0]
              ['maxProperties max-props #:default +inf.0]
              ['patternProperties pattern-props #:default #f]
              ['dependentRequired dependent-required #:default #f]
              ['dependentSchemas dependent-schemas #:default #f]
              ['additionalProperties additional-props #:default undefined]
              ['unevaluatedProperties unevaluated-props #:default undefined])}
       (define n (hash-count v))
     
       (define (object/check-size ctx)
         (cond
           [(<= min-props n max-props) ctx]
           [else
            (~> (hasheq 'minProperties min-props
                        'maxProperties max-props)
                (list v (reason/object-size-error v n min-props max-props))
                (apply invalidate-ctx ctx _))]))
     
       (define (object/check-required ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(subset? (map string->symbol required) (hash-keys v)) ctx]
           [else
            (~> (hasheq 'required required)
                (list v (reason/object-required-properties-error v required))
                (apply invalidate-ctx ctx _))]))

       (define (object/check-dependent-required ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not dependent-required) ctx]
           [else
            (for/fold ([ctx ctx]
                       #:result ctx)
                      ([(prop req) (in-immutable-hash dependent-required)]
                       #:break (not (validation-ctx-ok? ctx))
                       #:when (hash-has-key? v prop))
              (cond
                [(subset? (map string->symbol req) (hash-keys v)) ctx]
                [else
                 (~> (hasheq 'dependentRequired dependent-required)
                     (list v (reason/object-required-properties-error v req))
                     (apply invalidate-ctx ctx _))]))]))
       
       (define (object/check-dependent-schemas ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not dependent-schemas) ctx]
           [else
            (define ann (validation-ctx-annotations ctx))
            (for/fold ([ok? #t]
                       [err null]
                       #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([(prop schema) (in-immutable-hash dependent-schemas)]
                       #:when (hash-has-key? v prop)
                       #:break (not ok?))
              (define ctx* (validate/schema ok v schema))
              (if (validation-ctx-ok? ctx*)
                  (values ok? err)
                  (values #f (append err (validation-ctx-errors ctx*)))))]))
       
       (define (object/check-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not props) ctx]
           [else
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([(prop schema) (in-immutable-hash props)]
                       #:break (not ok?)
                       #:when (hash-has-key? v prop))
              (define ctx* (parameterize ([current-path (cons prop (current-path))])
                             (validate/schema ok (hash-ref v prop) schema)))
              (cond
                [(validation-ctx-ok? ctx*)
                 (values ok? (set-add ann prop) err)]
                [else
                 (values #f ann (append err (validation-ctx-errors ctx*)))]))]))

       (define (object/check-pattern-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(not pattern-props) ctx]
           [else
            (define pat (map symbol->regexp (hash-keys pattern-props)))
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([(prop u) (in-immutable-hash v)]
                       #:break (not ok?))
              (define schema
                (for/first ([rx (in-list pat)]
                            #:when (regexp-match? rx (symbol->string prop)))
                  (hash-ref pattern-props (regexp->symbol rx))))
              (cond
                [(not schema) (values ok? ann err)]
                [else
                 (define ctx* (parameterize ([current-path (cons prop (current-path))])
                                (validate/schema ok u schema)))
                 (cond
                   [(validation-ctx-ok? ctx*)
                    (values ok? (set-add ann prop) err)]
                   [else
                    (values #f ann (append err (validation-ctx-errors ctx*)))])]))]))

       (define (object/check-additional-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? additional-props) ctx]
           [else
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([(prop u) (in-immutable-hash v)]
                       #:break (not ok?)
                       #:unless (set-member? ann prop))
              (define ctx* (parameterize ([current-path (cons prop (current-path))])
                             (validate/schema ok u additional-props)))
              (cond
                [(validation-ctx-ok? ctx*)
                 (values ok? (set-add ann prop) err)]
                [else
                 (values #f ann (append err (validation-ctx-errors ctx*)))]))]))

       (define (object/check-unevaluated-properties ctx)
         (cond
           [(not (validation-ctx-ok? ctx)) ctx]
           [(undefined? unevaluated-props) ctx]
           [else
            (for/fold ([ok? #t]
                       [ann (validation-ctx-annotations ctx)]
                       [err null]
                       #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                      ([(prop u) (in-immutable-hash v)]
                       #:break (not ok?)
                       #:unless (set-member? ann prop))
              (define ctx* (parameterize ([current-path (cons prop (current-path))])
                             (validate/schema ok u unevaluated-props)))
              (cond
                [(validation-ctx-ok? ctx*)
                 (values ok? (set-add ann prop) err)]
                [else
                 (values #f ann (append err (validation-ctx-errors ctx*)))]))]))
     
       (~> ctx
           (object/check-size)
           (object/check-required)
           (object/check-dependent-required)
           (object/check-dependent-schemas)
           (object/check-properties)
           (object/check-pattern-properties)
           (object/check-additional-properties)
           (object/check-unevaluated-properties))]

      [{ctx _v _schema} ctx]))

  (define (reason/contradiction-error value invalid)
    (~> "the value ~a must not match ~a"
        (format
         (jsexpr->string value)
         (jsexpr->string invalid))))

  (define schema/check-not
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        v
        (hash* ['not invalid])}
       (cond
         [(not (validation-ctx-ok? (validate/schema ok null v invalid))) ctx]
         [else
          (~> (hasheq 'not invalid)
              (list v (reason/contradiction-error v invalid))
              (apply invalidate-ctx ctx _))])]
    
      [{ctx _v _schema} ctx]))

  (define schema/check-if
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        v
        (hash* ['if cond*]
               ['then then #:default #t]
               ['else else* #:default undefined])}
       (define ctx* (validate/schema ctx v cond*))
       (cond
         [(validation-ctx-ok? ctx*)
          (validate/schema ctx* v then)]
         [(undefined? else*) ctx]
         [else
          (validate/schema ctx v else*)])]
      
      [{ctx _v _schema} ctx]))
  
  (define schema/check-all-of
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        v
        (hash* ['allOf (list all ...)])}
       (for/fold ([ok? #t]
                  [ann (validation-ctx-annotations ctx)]
                  [err null]
                  #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                 ([schema (in-list all)]
                  #:break (not ok?))
         (define ctx* (validate/schema ok v schema))
         (cond
           [(validation-ctx-ok? ctx*)
            (values ok? (set-union ann (validation-ctx-annotations ctx*)) err)]
           [else
            (values #f ann (append err (validation-ctx-errors ctx*)))]))]

      [{ctx _v _schema} ctx]))

  (define schema/check-any-of
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        v
        (hash* ['anyOf (list any ...)])}
       (for/fold ([ok? #f]
                  [ann (validation-ctx-annotations ctx)]
                  [err null]
                  #:result (validation-ctx ok? (if ok? ann empty-set) (if ok? null err)))
                 ([schema (in-list any)]
                  #:break ok?)
         (define ctx* (validate/schema ok v schema))
         (cond
           [(validation-ctx-ok? ctx*)
            (values #t (set-union ann (validation-ctx-annotations ctx*)) err)]
           [else
            (values ok? ann (append err (validation-ctx-errors ctx*)))]))]
      
      [{ctx _v _schema} ctx]))

  (define schema/check-one-of
    (match-lambda**
      [{(? validation-ctx-ok? ctx)
        v
        (hash* ['oneOf (list one ...)])}
       (for/fold ([count 0]
                  [ann (validation-ctx-annotations ctx)]
                  [err null]
                  #:result
                  (cond
                    [(= 1 count) (validation-ctx #t ann null)]
                    [else
                     (validation-ctx #f empty-set err)]))
                 ([schema (in-list one)]
                  #:break (< 1 count))
         (define ctx* (validate/schema ok v schema))
         (cond
           [(validation-ctx-ok? ctx*)
            (values (+ count 1) (set-union ann (validation-ctx-annotations ctx*)) err)]
           [else
            (values count ann (append err (validation-ctx-errors ctx*)))]))]

      [{ctx _v _schema} ctx]))

  (define (validate/schema ctx value schema)
    (~> ctx
        (schema/check-literal value schema)
        (schema/check-type    value schema)
        (schema/check-const   value schema)
        (schema/check-number  value schema)
        (schema/check-string  value schema)
        (schema/check-not     value schema)
        (schema/check-if      value schema)
        (schema/check-all-of  value schema)
        (schema/check-any-of  value schema)
        (schema/check-one-of  value schema)
        (schema/check-array   value schema)
        (schema/check-object  value schema))))

(require 'schema-utils)

(define None    #f)
(define Some    (hasheq))
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

(define (Not schema) (&not Some schema))
(define (Just value) (&const Some value))

(define (AllOf #:in [schema Some] . rest) (&all-of schema rest))
(define (AnyOf #:in [schema Some] . rest) (&any-of schema rest))
(define (OneOf #:in [schema Some] . rest) (&one-of schema rest))

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

(define (json/check-schema value schema)
  (define ctx (parameterize ([current-path null])
                (validate/schema ok value schema)))
  (if (validation-ctx-ok? ctx) #f (validation-ctx-errors ctx)))

#;
(json/check-schema
 '(2 2 "3" (1 2))
 (with-Array Some
   #:prefix-items
   (list
    Number
    (Or Number String)
    (Or Number String)
    (with-Array (Array Some)
      #:min-items 2))
   #:contains Number
   #:max-contains 3
   #:min-contains 2))
#;
(list
 (schema-error
  '(3)
  '#hasheq((maxItems . +inf.0) (minItems . 2))
  '(1)
  "the array [1] has less than 2 items"))