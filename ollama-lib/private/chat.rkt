#lang racket/base

(provide
  with-ollama-chat)

(define-syntax-rule
  (with-ollama-chat [(more continue) start-chat]
    . body)
  (let-values ([(more continue) start-chat])
    (let loop ([more more] [continue continue])
      (let ([continue (compose loop continue)])
        . body))))