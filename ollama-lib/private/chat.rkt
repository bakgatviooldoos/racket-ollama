#lang racket/base

(require
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

(define &content (&hash-ref* 'message 'content))
(define &thinking (&opt-hash-ref* 'message 'thinking))
(define &tool-calls (&opt-hash-ref* 'message 'tool_calls))
(define &image (&opt-hash-ref 'image))
(define &status (&hash-ref 'status))
(define &digest (&opt-hash-ref 'digest))
(define &total (&opt-hash-ref 'total))
(define &completed (&opt-hash-ref 'completed))

(define-syntax-rule
  (with-tool-calls [(more* calls) more]
    . body)
  (let ([more* more])
    (for/fold ([calls null]
               [!call #f]
               #:result
               (cond
                 [(string=? "" (&content !call))
                  . body]
                 [else
                  (let ([more*
                         (lambda ()
                           (begin0 !call
                             (set! !call (more*))))])
                    . body)]))
              ([part (in-producer more* eof)]
               #:do [(define tools (&tool-calls part))]
               #:final (not tools))
      (if (not tools)
          (values calls part)
          (values (append calls tools) !call)))))

(define-syntax-rule
  (with-thinking [(more* thinks) more]
    . body)
  (let ([more* more])
    (for/fold ([thinks null]
               [!think #f]
               #:result
               (let ([thinks (string-join (reverse thinks) "")])
                 (cond
                   [(string=? "" (&content !think))
                    . body]
                   [else
                    (let ([more*
                           (lambda ()
                             (begin0 !think
                               (set! !think (more*))))])
                      . body)])))
              ([part (in-producer more* eof)]
               #:do [(define thinking (&thinking part))]
               #:final (not thinking))
      (if (not thinking)
          (values thinks part)
          (values (cons thinking thinks) !think)))))

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
          [completed (&completed part)]
          [total (&total part)])
      . body)))

(define-syntax-rule
  (with-push-model [(status digest total) more]
    . body)
  (for ([part (in-producer more eof)])
    (let ([status (&status part)]
          [digest (&digest part)]
          [total (&total part)])
      . body)))

(define make-message #f)
(define call-tool #f)

(with-ollama-chat [(more continue) 'start]
  (with-tool-calls [(more calls) more]
    (cond
      [(null? calls)
       (with-thinking [(more thinks) more]
         (displayln thinks)
         (for ([data (in-producer more eof)])
           (displayln data))
         (continue "more, more!"))]
      [else
       (continue
        (for/list ([data (in-list calls)])
          (make-message
           #:role 'tool
           (call-tool data))))])))