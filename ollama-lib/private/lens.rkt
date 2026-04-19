#lang racket/base

(require data/monocle
         racket/mutable-treelist
         racket/string
         threading)

(provide
 (all-defined-out))

(define &message.content (&hash-ref* 'message 'content))
(define &message.thinking (&opt-hash-ref* 'message 'thinking))
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
(define &token (&hash-ref 'token))
(define &logprob (&hash-ref 'logprob))
(define &bytes (&hash-ref 'bytes))
(define &top-logprobs (&hash-ref 'top_logprobs))
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
 
(define (parts->complete-message parts)
  (define-values (stat contents)
    (for/fold ([stat zero-stat] ;; noqa
               [contents null]
               #:result
               (values
                (zero-stat . stat+ . stat)
                (reverse contents)))
            ([part (in-mutable-treelist parts)])
      (define content (&message.content part))
      (values part (cons content contents))))
  (~>> (string-append* contents)
       (hasheq 'content)
       (hash-set stat 'message)))

(define (parts->complete-response parts)
  (define-values (stat responses)
    (for/fold ([stat zero-stat] ;; noqa
               [responses null]
               #:result
               (values
                (zero-stat . stat+ . stat)
                (reverse responses)))
              ([part (in-mutable-treelist parts)])
      (define response (&response part))
      (values part (cons response responses))))
  (~>> (string-append* responses)
       (hash-set stat 'response)))
