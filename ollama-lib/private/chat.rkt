#lang racket/base

(require
  (for-syntax racket/base)
  data/monocle
  racket/string)

(provide
  with-ollama-chat
  with-tool-calls)

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

(define (prepend-part part more*)
  (unless part (set! part (more*)))
  (lambda ()
    (begin0 part
      (set! part (more*)))))

(define (extract-tool-calls more*)
  (for/fold ([calls null]
             [!call #f]
             #:result
             (values
              (prepend-part !call more*)
              (reverse calls)))
            ([part (in-producer more* eof)]
             #:do [(define tools (&message.tool-calls part))]
             #:final (not tools))
    (if (not tools)
        (values calls part)
        (values (append calls tools) !call))))

(define-syntax-rule
  (with-tool-calls [(more* calls) more]
    . body)
  (let-values ([(more* calls) (extract-tool-calls more)])
    . body))

(define (extract-thinking
         #:key [&thinking &message.thinking]
         more*)
  (for/fold ([thinks null]
             [!think #f]
             #:result
             (values
              (prepend-part !think more*)
              (string-append* (reverse thinks))))
            ([part (in-producer more* eof)]
             #:do [(define thinking (&thinking part))]
             #:final (not thinking))
    (if (not thinking)
        (values thinks part)
        (values (cons thinking thinks) !think))))

(define-syntax with-thinking
  (syntax-rules ()
    [(_ [(more* thinks) #:chat more]
        . body)
     (with-thinking [(more* thinks) more]
       . body)]

    [(_ [(more* thinks) #:response more]
        . body)
     (let-values ([(more* thinks) (extract-thinking more #:key &thinking)])
       . body)]

    [(_ [(more* thinks) more]
        . body)
     (let-values ([(more* thinks) (extract-thinking more)])
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

(define (extract-content-string more)
  (string-append*
   (for/list ([part (in-producer more eof)])
     (&message.content part))))

(define (extract-response-string more)
  (string-append*
   (for/list ([part (in-producer more eof)])
     (&response part))))

(define (->labeled-chat-response more)
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

(define (->labeled-generate-response more)
  (lambda ()
    (let ([part (more)])
      (cond
        [(eof-object? part)
         (values #f eof)]
        [(&thinking part)
         (values 'thinking part)]
        [(non-empty-string? (&response part))
         (values 'content part)]
        [else
         (values 'done part)]))))

(define-sequence-syntax in-chat-response
  (lambda (stx) #'->labeled-chat-response)
  (lambda (stx)
    (syntax-case stx ()
      [[(label part) (_ more)]
       #'[(label part) (in-producer more (lambda (l p) (eof-object? p)))]])))

(define-sequence-syntax in-generate-response
  (lambda (stx) #'->labeled-generate-response)
  (lambda (stx)
    (syntax-case stx ()
      [[(label part) (_ more)]
       #'[(label part) (in-producer more (lambda (l p) (eof-object? p)))]])))

(define make-message #f)
(define call-tool #f)

(with-ollama-chat [(more continue) 'start]
  (with-tool-calls [(more calls) more]
    (cond
      [(null? calls)
       (with-thinking [(more thinks) more]
         (displayln thinks)
         (displayln (extract-content-string more))
         (continue "more, more!"))]
      [else
       (continue
        (for/list ([data (in-list calls)])
          (make-message
           #:role 'tool
           (call-tool data))))])))