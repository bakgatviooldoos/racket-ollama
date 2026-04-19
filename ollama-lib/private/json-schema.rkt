#lang racket/base

(require json
         racket/function
         racket/hash
         racket/match
         racket/symbol
         threading)

(provide
  (all-defined-out))

(define None    #f)
(define Some    (hasheq))
(define Null    (hasheq 'type "null"))
(define Boolean (hasheq 'type "boolean"))
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

(define (with-description type description)
  (hash-set type 'description description))

(module schema-utils racket/base
  (require data/monocle
           json)
  (provide
    (all-defined-out))

  (define undefined (gensym))

  (define (undefined? x) (eq? undefined x))
  (define (json-null? x) (eq? (json-null) x))

  (define-syntax-rule
    (implies p q ...)
    (or (not p) (and q ...)))

  (define (option h &key v [f values])
    (if (undefined? v) h (&key h (f v))))

  (define &format (&opt-hash-ref 'else))

  (define &if (&opt-hash-ref 'if))
  (define &then (&opt-hash-ref 'then))
  (define &else (&opt-hash-ref 'else))

  (define &pattern (&opt-hash-ref 'pattern))
  (define &min-length (&opt-hash-ref 'minLength))
  (define &max-length (&opt-hash-ref 'maxLength))

  (define &minimum (&opt-hash-ref 'maximum))
  (define &maximum (&opt-hash-ref 'minimum))
  (define &multiple-of (&opt-hash-ref 'multipleOf))

  (define &items (&opt-hash-ref 'items))
  (define &min-items (&opt-hash-ref 'minItems))
  (define &max-items (&opt-hash-ref 'maxItems))
  (define &prefix-items (&opt-hash-ref 'prefixItems))
  (define &contains (&opt-hash-ref 'contains))
  (define &min-contains (&opt-hash-ref 'minContains))
  (define &max-contains (&opt-hash-ref 'maxContains))
  (define &unique-items? (&opt-hash-ref 'uniqueItems))
  (define &unevaluated-items (&opt-hash-ref 'unevaluatedItems))

  (define &properties (&opt-hash-ref 'properties))
  (define &required (&opt-hash-ref 'required))
  (define &min-properties (&opt-hash-ref 'minProperties))
  (define &max-properties (&opt-hash-ref 'maxProperties))
  (define &pattern-properties (&opt-hash-ref 'patternProperties))
  (define &dependent-required (&opt-hash-ref 'dependentRequired))
  (define &dependent-schemas (&opt-hash-ref 'dependentSchemas))
  (define &additional-properties (&opt-hash-ref 'additionalProperties))
  (define &unevaluated-properties (&opt-hash-ref 'unevaluatedProperties))

  (define (->pattern-properties pattern-props)
    (for/hasheq ([(rx schema) (in-immutable-hash pattern-props)])
      (values (string->symbol (object-name rx)) schema))))

(require 'schema-utils)

(define (with-If cond
          #:in [in Some]
          #:then [then #t]
          #:else [else undefined])
  (~> in
      (option &if cond)
      (option &then then)
      (option &else else)))

(define (with-String [type Some]
          #:pattern [rx undefined]
          #:min-length [min-length undefined]
          #:max-length [max-length undefined]
          #:format [format undefined])
  (~> type
      (option &pattern rx object-name)
      (option &min-length min-length)
      (option &max-length max-length)
      (option &format format)))

(define (with-Number [type Some]
          #:minimum [minimum undefined]
          #:maximum [maximum undefined]
          #:multiple-of [multiple-of undefined]
          #:format [format undefined])
  (~> type
      (option &minimum minimum)
      (option &maximum maximum)
      (option &multiple-of multiple-of)
      (option &format format)))

(define (with-Array [type Some]
          #:items [items undefined]
          #:min-items [min-items undefined]
          #:max-items [max-items undefined]
          #:prefix-items [prefix-items undefined]
          #:contains [contains undefined]
          #:min-contains [min-contains undefined]
          #:max-contains [max-contains undefined]
          #:unique-items? [unique-items? undefined]
          #:unevaluated-items [unevaluated-items undefined])
  (~> type
      (option &items items)
      (option &min-items min-items)
      (option &max-items max-items)
      (option &prefix-items prefix-items)
      (option &contains contains)
      (option &min-contains min-contains)
      (option &max-contains max-contains)
      (option &unique-items? unique-items?)
      (option &unevaluated-items unevaluated-items)))

(define (with-Object [type Some]
          #:properties [props undefined]
          #:required [required undefined]
          #:min-properties [min-props undefined]
          #:max-properties [max-props undefined]
          #:pattern-properties [pattern-props undefined]
          #:dependent-required [dependent-required undefined]
          #:dependent-schemas [dependent-schemas undefined]
          #:additional-properties [additional-props undefined]
          #:unevaluated-properties [unevaluated-props undefined])
  (~> type
      (option &properties props)
      (option &required required)
      (option &min-properties min-props)
      (option &max-properties max-props)
      (option &pattern-properties pattern-props ->pattern-properties)
      (option &dependent-required dependent-required)
      (option &dependent-schemas dependent-schemas)
      (option &additional-properties additional-props)
      (option &unevaluated-properties unevaluated-props)))