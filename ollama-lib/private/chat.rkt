#lang racket/base

(require
  (for-syntax racket/base)
  data/monocle
  racket/string
  threading
  "lens.rkt"
  "tool.rkt"
  "message.rkt")

(provide
  capture-tool-calls
  capture-tool-calls+stats
  capture-thinking/message
  capture-thinking/response
  capture-content
  capture-content+stats
  capture-response
  capture-response+stats
  in-producer/message
  in-producer/response
  with-ollama-chat
  with-content
  with-response
  with-thinking
  with-tool-calls
  with-thinking+content
  with-thinking+response
  with-tool-calls+thinking+content
  with-generate-image
  with-create-model
  with-pull-model
  with-push-model)

(define-syntax-rule
  (with-ollama-chat [(more continue) start-chat]
    . body)
  (let-values ([(more continue) start-chat])
    (let loop ([more more] [continue continue])
      (let ([continue (compose loop continue)])
        . body))))

(define &message.content (&hash-ref* 'message 'content))
(define &message.thinking (&opt-hash-ref* 'message 'thinking))
(define &message.tool-calls (&opt-hash-ref* 'message 'tool_calls))
(define &response (&hash-ref 'response))
(define &thinking (&opt-hash-ref 'thinking))
(define &image (&opt-hash-ref 'image))
(define &status (&hash-ref 'status))
(define &digest (&opt-hash-ref 'digest))
(define &total (&opt-hash-ref 'total))
(define &completed (&opt-hash-ref 'completed))

(define (prepend-part part more)
  (unless part (set! part (more)))
  (lambda ()
    (begin0 part
      (set! part (more)))))

(define (capture-tool-calls more)
  (for/fold ([calls null]
             [!call #f]
             #:result
             (values
              (prepend-part !call more)
              (reverse calls)))
            ([part (in-producer more eof)]
             #:do [(define tools (&message.tool-calls part))]
             #:final (not tools))
    (if (not tools)
        (values calls part)
        (values (append calls tools) !call))))

(define (capture-tool-calls+stats more)
  (let-values ([(more calls) (capture-tool-calls more)])
    (if (null? calls)
        (values more calls #f)
        (values more calls (more)))))

(define-syntax with-tool-calls
  (syntax-rules ()
    [(_ [(more* calls) more]
        . body)
     (let-values ([(more* calls) (capture-tool-calls more)])
       . body)]

    [(_ [(more* calls stats) more]
        . body)
     (let-values ([(more* calls stats) (capture-tool-calls+stats more)])
       . body)]))

(define (reverse/string-append* ss)
  (string-append* (reverse ss)))

(define (capture-thinking more #:key &thinking)
  (for/fold ([thinks null]
             [!think #f]
             #:result
             (values
              (prepend-part !think more)
              (reverse/string-append* thinks)))
            ([part (in-producer more eof)]
             #:do [(define thinking (&thinking part))]
             #:final (not thinking))
    (if (not thinking)
        (values thinks part)
        (values (cons thinking thinks) !think))))

(define (capture-thinking/message more)
  (capture-thinking #:key &message.thinking))

(define (capture-thinking/response more)
  (capture-thinking #:key &thinking))

(define-syntax with-thinking
  (syntax-rules ()
    [(_ [(more* thinks) more]
        . body)
     (let-values ([(more* thinks) (capture-thinking/message more)])
       . body)]

    [(_ [(more* thinks) #:message more]
        . body)
     (with-thinking [(more* thinks) more]
       . body)]

    [(_ [(more* thinks) #:response more]
        . body)
     (let-values ([(more* thinks) (capture-thinking/response more)])
       . body)]))

(define-syntax-rule
  (with-generate-image [(total completed) more]
    . body)
  (for/last ([part (in-producer more eof)])
    (cond
      [(&image part) part]
      [else
       (let ([completed (&completed part)]
             [total (&total part)])
         . body)])))

(define-syntax-rule
  (with-create-model [status more]
    . body)
  (for ([part (in-producer more eof)])
    (let ([status (&status part)])
      . body)))

(define-syntax-rule
  (with-pull-model [(status digest total completed) more]
    . body)
  (for ([part (in-producer more eof)])
    (let ([status (&status part)]
          [digest (&digest part)]
          [total (&total part)]
          [completed (&completed part)])
      . body)))

(define-syntax-rule
  (with-push-model [(status digest total) more]
    . body)
  (for ([part (in-producer more eof)])
    (let ([status (&status part)]
          [digest (&digest part)]
          [total (&total part)])
      . body)))

(define (capture-text more #:key &text)
  (string-append*
   (for/list ([part (in-producer more eof)])
     (&text part))))

(define (capture-text+stats more #:key &text)
  (for/fold ([stat zero-stat] ;; noqa
             [contents null]
             #:result (values (reverse/string-append* contents) stat))
            ([part (in-producer more eof)])
    (values
     (stat . stat+ . part)
     (cons (&text part) contents))))

(define (capture-content more)
  (capture-text more #:key &message.content))

(define (capture-content+stats more)
  (capture-text+stats more #:key &message.content))

(define (capture-response more)
  (capture-text more #:key &response))

(define (capture-response+stats more)
  (capture-text+stats more #:key &response))

(define-syntax with-content
  (syntax-rules ()
    [(_ [(content stats) more]
        . body)
     (let-values ([(content stats) (capture-content+stats more)])
       . body)]

    [(_ [content more]
        . body)
     (let-values ([(content _) (capture-content+stats more)])
       . body)]))

(define-syntax with-response
  (syntax-rules ()
    [(_ [(content stats) more]
       . body)
     (let-values ([(content stats) (capture-response+stats more)])
       . body)]

    [(_ [content more]
       . body)
     (let-values ([(content _) (capture-response+stats more)])
       . body)]))

(define-syntax with-thinking+content
  (syntax-rules ()
    [(_ [(thinks content stats) more]
        . body)
     (with-thinking [(more thinks) more]
       (with-content [(content stats) more]
         . body))]

    [(_ [(thinks content) more]
        . body)
     (with-thinking [(more thinks) more]
       (with-content [content more]
         . body))]))

(define-syntax with-tool-calls+thinking+content
  (syntax-rules ()
    [(_ [(calls thinks content stats) more]
        . body)
     (with-tool-calls [(more calls) more]
       (with-thinking+content [(thinks content stats) more]
         . body))]

    [(_ [(thinks content) more]
        . body)
     (with-tool-calls [(more calls) more]
       (with-thinking+content [(thinks content) more]
         . body))]))

(define-syntax with-thinking+response
  (syntax-rules ()
    [(_ [(thinks content stats) more]
        . body)
     (with-thinking [(more thinks) more]
       (with-response [(content stats) more]
         . body))]
    
    [(_ [(thinks content) more]
        . body)
     (with-thinking [(more thinks) more]
       (with-response [content more]
         . body))]))

(define (->labeled-producer/message more)
  (lambda ()
    (let ([part (more)])
      (cond
        [(eof-object? part)
         (values #f eof)]
        [(&message.tool-calls part)
         (values 'tool-calls part)]
        [(&message.thinking part)
         (values 'thinking part)]
        [(non-empty-string? (&message.content part))
         (values 'content part)]
        [else
         (values 'done part)]))))

(define (->labeled-producer/response more)
  (lambda ()
    (let ([part (more)])
      (cond
        [(eof-object? part)
         (values #f eof)]
        [(&thinking part)
         (values 'thinking part)]
        [(non-empty-string? (&response part))
         (values 'response part)]
        [else
         (values 'done part)]))))

(define-sequence-syntax in-producer/message
  (lambda (stx) #'->labeled-producer/message)
  (lambda (stx)
    (syntax-case stx ()
      [[(label part) (_ more)]
       #'[(label part) (in-producer
                        (->labeled-producer/message more)
                        (lambda (l p) (eof-object? p)))]])))

(define-sequence-syntax in-producer/response
  (lambda (stx) #'->labeled-producer/response)
  (lambda (stx)
    (syntax-case stx ()
      [[(label part) (_ more)]
       #'[(label part) (in-producer
                        (->labeled-producer/response more)
                        (lambda (l p) (eof-object? p)))]])))

(define call-tool #f)

(with-ollama-chat [(more continue) 'start]
  (with-tool-calls [(more calls) more]
    (cond
      [(null? calls)
       (with-thinking+content [(thinks content) more]
         (displayln (format "thinking: ~a" thinks))
         (displayln (format "contents: ~a" content))
         (continue "more, more!"))]
      [else
       (continue
        (for/list ([data (in-list calls)])
          (make-message
           #:role 'tool
           (call-tool data))))])))