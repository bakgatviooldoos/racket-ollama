#lang racket/base

(provide
  with-ollama-chat)

(define-syntax with-ollama-chat
  (syntax-rules ()
    [(_ [(more continue) start-chat]
        #:declare ([arg init] ...)
        . body)
     (let ([arg (make-parameter #f)] ...)
       (parameterize ([arg init] ...)
         (with-ollama-chat [(more continue) start-chat]
           . body)))]
    
    [(_ [(more continue) start-chat]
        . body)
     (let-values ([(more continue) start-chat])
       (let loop ([more more] [continue continue])
         (let ([continue (compose loop continue)])
           . body)))]))