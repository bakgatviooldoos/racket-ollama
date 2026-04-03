#lang racket/base

(require json
         racket/hash)

(provide
  (all-defined-out))

(define (json-null? x) (eq? (json-null) x))

(define (?? ?x y) (if (json-null? ?x) y ?x))
(define (.? ?x f) (if (json-null? ?x) ?x (f ?x)))

(define (jsonopt . nullable)
  (compose1
   (lambda (hash)
     (hash-filter hash
      (lambda (k v)
        (or (not (json-null? v)) (memq k nullable)))))
   hasheq))