#lang racket/base

(require data/monocle
         racket/mutable-treelist
         racket/string
         threading)

(provide
 (all-defined-out))

(define &content (&hash-ref* 'message 'content))
(define &thinking* (&opt-hash-ref* 'message 'thinking))
(define &response (&hash-ref 'response))
(define &thinking (&opt-hash-ref 'thinking))
(define &total-duration (&opt-hash-ref 'total_duration))
(define &load-duration (&opt-hash-ref 'load_duration))
(define &prompt-eval-count (&opt-hash-ref 'prompt_eval_count))
(define &prompt-eval-duration (&opt-hash-ref 'prompt_eval_duration))
(define &eval-count (&opt-hash-ref 'eval_count))
(define &eval-duration (&opt-hash-ref 'eval_duration))
(define &done-reason (&opt-hash-ref 'done_reason))
(define &logprobs (&opt-hash-ref 'logprobs))
(define &model (&opt-hash-ref 'model))
(define &created-at (&opt-hash-ref 'created_at))

(define zero-stat
  (~> (hasheq)
      (&total-duration 0)
      (&load-duration 0)
      (&prompt-eval-count 0)
      (&prompt-eval-duration 0)
      (&eval-count 0)
      (&eval-duration 0)))

(define (stat+ s data)
  (define ((make-adder &lens) x)
    (x . + . (or (&lens data) 0)))
  (define (update s &lens) ;; noqa
    (lens-update &lens s (make-adder &lens)))
  (~> s
      (update &total-duration)
      (update &load-duration)
      (update &prompt-eval-count)
      (update &prompt-eval-duration)
      (update &eval-count)
      (update &eval-duration)))

(define ((think+ think? &thinking) thinking data)
  (cond
    [(and think? (&thinking data))
     => (lambda~> (cons thinking))]
    [else thinking]))

(define (message-parts->complete-message
         #:chat? [chat? #t]
         #:think? [think? #f]
         parts)
  (define-values (stat thinking contents done-reason logprobs) ;; noqa
    (let ([&content (if chat? &content &response)]
          [think+ (think+ think? (if chat? &thinking* &thinking))])
      (for/fold ([stat zero-stat] ;; noqa
                 [thinking null]
                 [contents null]
                 [done-reason #f]
                 [logprobs #f]
                 #:result
                 (values
                  stat
                  (reverse thinking)
                  (reverse contents)))
                ([part (in-mutable-treelist parts)])
        (values
         (stat . stat+ . part)
         (thinking . think+ . part)
         (cons (&content part) contents)
         (&done-reason part)
         (&logprobs part)))))
  (~> (hasheq
       'thinking (string-join thinking "")
       'content (string-join contents ""))
      (hash-set stat
       'message _
       'done_reason done-reason
       'logprobs logprobs)))
