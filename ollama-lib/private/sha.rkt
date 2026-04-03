#lang racket/base

(require
  (only-in file/sha1
           hex-string->bytes
           bytes->hex-string)
  (only-in sha
           sha256?))

(provide
  sha256-string?
  bytes->hex-string)

(define (sha256-string? hex)
  (sha256? (hex-string->bytes hex)))
