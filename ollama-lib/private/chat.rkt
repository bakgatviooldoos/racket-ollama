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

(define (capture-tool-calls/stats more)
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
     (let-values ([(more* calls stats) (capture-tool-calls/stats more)])
       . body)]))

(define (raise-tool-not-found-error name data)
  (raise
   (exn:fail:tool:not-found
    (format "tool '~a' does not exist" name)
    (current-continuation-marks)
    #;data data
    #;hints '("check the tool list again and retry"))))

;; toolkit:
;; way to manage the tools available to the model at any given point in the chat
;; maybe provide a way to combine/filter tools from different tool-definers
;; maybe provide a caller which accepts tool-calls from this 'toolkit'

;; work-in-progress, needs refinement
(define ((make-toolkit . caller-map)
         #:to-message? [to-message? #f]
         data)
  (define name (string->symbol (hash-ref data 'name)))
  (cond
    [(for/first ([caller/ids (in-list caller-map)]
                 #:when (memq name (cdr caller/ids)))
       (define result ((car caller/ids) data))
       (if to-message?
           (make-message
            #:role 'tool
            result)
           result))]
    [else
     (raise-tool-not-found-error name data)]))

;; work-in-progress, needs refinement
(define-syntax-rule
  (define-toolkit caller*
    [caller (id ...)] ...)
  (define (caller* data #:to-message? [to-message? #f])
    (define result
      (define name (string->symbol (hash-ref data 'name)))
      (case name
        [(id ...) (caller data)]
        ...
        [else
         (raise-tool-not-found-error name data)]))
    (if to-message?
        (make-message
         #:role 'tool
         result)
        result)))

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

(define (capture-thinking-from-message more)
  (capture-thinking #:key &message.thinking))

(define (capture-thinking-from-response more)
  (capture-thinking #:key &thinking))

(define-syntax with-thinking
  (syntax-rules ()
    [(_ [(more* thinks) more]
        . body)
     (let-values ([(more* thinks) (capture-thinking-from-message more)])
       . body)]

    [(_ [(more* thinks) #:message more]
        . body)
     (with-thinking [(more* thinks) more]
       . body)]

    [(_ [(more* thinks) #:response more]
        . body)
     (let-values ([(more* thinks) (capture-thinking-from-response more)])
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

(define (capture-content-string more)
  (string-append*
   (for/list ([part (in-producer more eof)])
     (&message.content part))))

(define (capture-content-string/stats more)
  (for/fold ([stat zero-stat] ;; noqa
             [contents null]
             #:result (values (reverse/string-append* contents) stat))
            ([part (in-producer more eof)])
    (values
     (stat . stat+ . part)
     (cons (&message.content part) contents))))

(define (capture-response-string more)
  (string-append*
   (for/list ([part (in-producer more eof)])
     (&response part))))

(define (capture-response-string/stats more)
  (for/fold ([stat zero-stat] ;; noqa
             [response null]
             #:result (values (reverse/string-append* response) stat))
            ([part (in-producer more eof)])
    (values
     (stat . stat+ . part)
     (cons (&response part) response))))

(define-syntax with-content
  (syntax-rules ()
    [(_ [(content stats) more]
        . body)
     (let-values ([(content stats) (capture-content-string/stats more)])
       . body)]

    [(_ [content more]
        . body)
     (let-values ([(content _) (capture-content-string/stats more)])
       . body)]))

(define-syntax with-response
  (syntax-rules ()
    [(_ [(content stats) more]
       . body)
     (let-values ([(content stats) (capture-response-string/stats more)])
       . body)]

    [(_ [content more]
       . body)
     (let-values ([(content _) (capture-response-string/stats more)])
       . body)]))

(define-syntax with-thinking/content
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

(define-syntax with-tool-calls/thinking/content
  (syntax-rules ()
    [(_ [(calls thinks content stats) more]
        . body)
     (with-tool-calls [(more calls) more]
       (with-thinking/content [(thinks content stats) more]
         . body))]

    [(_ [(thinks content) more]
        . body)
     (with-tool-calls [(more calls) more]
       (with-thinking/content [(thinks content) more]
         . body))]))

(define-syntax with-thinking/response
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
         (values 'response part)]
        [else
         (values 'done part)]))))

(define-sequence-syntax in-producer/message
  (lambda (stx) #'->labeled-chat-response)
  (lambda (stx)
    (syntax-case stx ()
      [[(label part) (_ more)]
       #'[(label part) (in-producer
                        (->labeled-chat-response more)
                        (lambda (l p) (eof-object? p)))]])))

(define-sequence-syntax in-producer/response
  (lambda (stx) #'->labeled-generate-response)
  (lambda (stx)
    (syntax-case stx ()
      [[(label part) (_ more)]
       #'[(label part) (in-producer
                        (->labeled-generate-response more)
                        (lambda (l p) (eof-object? p)))]])))

(define call-tool #f)

(with-ollama-chat [(more continue) 'start]
  (with-tool-calls [(more calls) more]
    (cond
      [(null? calls)
       (with-thinking/content [(thinks content) more]
         (displayln (format "thinking: ~a" thinks))
         (displayln (format "contents: ~a" content))
         (continue "more, more!"))]
      [else
       (continue
        (for/list ([data (in-list calls)])
          (make-message
           #:role 'tool
           (call-tool data))))])))