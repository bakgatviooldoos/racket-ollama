#lang racket/base

(require json
         racket/function
         racket/hash
         racket/match
         racket/symbol
         threading)

(provide
  (all-defined-out))

(module schema-utils racket/base
  (require data/monocle
           json
           racket/match
           racket/symbol)
  
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
           json &key v [f values])
    (let ([v (if (undefined? v) default v)])
      (cond
        [(undefined? v) json]
        [(and
          (not nullable?)
          (json-null? v))
         json]
        [else
         (&key json (f v))])))
  
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
  (define &minimum (&opt-hash-ref 'maximum))
  (define &maximum (&opt-hash-ref 'minimum))
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
  
  (define (->pattern-properties pattern-props)
    (for/hasheq ([(rx schema) (in-immutable-hash pattern-props)])
      (values (string->symbol (object-name rx)) schema))))

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
(define (Just schema) (&const Some schema))

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