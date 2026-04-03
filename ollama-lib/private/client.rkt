#lang racket/base

(require json
         json/to-jsexpr
         net/http-easy
         racket/mutable-treelist
         racket/string
         racket/treelist
         struct-define
         threading
         "json.rkt"
         "lens.rkt"
         "message.rkt"
         "tool.rkt")

(provide
 make-ollama-client
 ollama-client?
 ollama-start-chat)

(define timeouts
  (make-timeout-config
   #:request (* 5 60)))

(struct ollama-client (auth session ~endpoint))

(define (make-ollama-client
         #:auth [auth (λ (_url headers params)
                        (values headers params))]
         [root "http://127.0.0.1:11434"])
  (ollama-client
   #;auth auth
   #;session (current-session)
   #;~endpoint (lambda args
                 (format "~a/~a" root (string-join args "/")))))

;; GENERATE
(define (ollama-generate
         #:suffix [suffix (json-null)]
         #:images [images (json-null)]
         #:format [output-format (json-null)]
         #:system [system-prompt (json-null)]
         #:think? [think? (json-null)]
         #:raw? [raw? (json-null)]
         #:keep-alive [keep-alive (json-null)]
         #:options [options (json-null)]
         #:response->message
         [response->message
          (lambda (data)
            (make-message
             #:role 'assistant
             (&content data)))]
         #:message-callback
         [message-callback void]
         c model [user-prompt #f])
  #f)

;; CHAT
(define (ollama-start-chat
         #:options [options (hasheq)]
         #:format [output-format #f]
         #:tools [tools #f]
         #:response->history-entry
         [response->history-entry
          (lambda (data)
            (make-message
             #:role 'assistant
             (&content data)))]
         c model str-or-messages)
  (struct-define ollama-client c)
  (let loop ([messages (ensure-messages str-or-messages)]
             [output-format output-format]
             [tools tools])
    (define done? #f)
    (define parts (mutable-treelist))
    (define resp
      (~> (session-request
           #:method 'post
           #:stream? #t
           #:auth auth
           #:json (hasheq
                   'model model
                   'stream #t
                   'options options
                   'messages (->jsexpr messages)
                   'tools (if tools
                              (->jsexpr (hash-values tools))
                              (json-null))
                   'format (if output-format
                               (->jsexpr output-format)
                               (json-null)))
           #:timeouts timeouts
           session (~endpoint "api" "chat"))
          (check-response 'ollama-chat _)))
    (let ([messages (treelist-copy messages)])
      (values
       (lambda ()
         (define data (read-json (response-output resp)))
         (cond
           [(eof-object? data)
            (unless done?
              (set! done? #t)
              (define complete-message
                (message-parts->complete-message parts))
              (unless (string=? (&content complete-message) "")
                (and~>
                 (response->history-entry complete-message)
                 (mutable-treelist-add! messages _))))
            (begin0 eof
              (response-close! resp))]
           [else
            (begin0 data
              (mutable-treelist-add! parts data))]))
       (lambda (#:format [output-format output-format] ;; noqa
                #:tools [tools tools] ;; noqa
                next-message)
         (if (list? next-message)
             (mutable-treelist-append! messages (ensure-messages next-message))
             (mutable-treelist-add! messages (ensure-message next-message)))
         (loop (mutable-treelist-snapshot messages) output-format tools))))))

;; EMBEDDINGS
(define (ollama-embed
         #:truncate? [truncate? (json-null)]
         #:dimensions [dimensions (json-null)]
         #:options [options (json-null)]
         #:keep-alive [keep-alive (json-null)]
         c model input)
  #f)

;; MODELS
(define (ollama-list-models c)
  #f)

(define (ollama-list-running c)
  #f)

(define (ollama-load-model client model)
  #f)

(define (ollama-unload-model client model)
  #f)

(define (ollama-show-model
         #:verbose? [verbose? #f]
         c model)
  #f)

(define (ollama-create-model
         #:from [from (json-null)]
         #:files [files (json-null)]
         #:adapters [adapters (json-null)]
         #:template [template (json-null)]
         #:license [license (json-null)]
         #:system [system (json-null)]
         #:parameters [parameters (json-null)]
         #:messages [messages (json-null)]
         #:stream? [stream? #t]
         #:quantize [quantize (json-null)]
         c model)
  #f)

(define (ollama-copy-model c model destination)
  #f)

(define (ollama-pull-model
         #:insecure? [insecure? #f]
         #:stream? [stream? #t]
         c model)
  #f)

(define (ollama-push-model
         #:insecure? [insecure? #f]
         #:stream? [stream? #t]
         c model)
  #f)

(define (ollama-delete-model c model)
  #f)

;; VERSION
(define (ollama-version c)
  #f)

;; STATUS (UNDOCUMENTED)
(define (ollama-online? c)
  #f)

(define (ollama-status c)
  #f)

;; BLOBS
(define (ollama-has-blob? c sha256)
  #f)

(define (ollama-upload-blob
         #:sha256 [sha256 #f]
         c data)
  #f)

(define (ensure-messages str-or-messages)
  (cond
    [(list? str-or-messages)
     (apply treelist (map ensure-message str-or-messages))]
    [(message? str-or-messages)
     (treelist str-or-messages)]
    [else
     (treelist (make-message str-or-messages))]))

(define (ensure-message str-or-message)
  (if (message? str-or-message)
      str-or-message
      (make-message str-or-message)))

(define (check-response who resp [ok '(200)])
  (begin0 resp
    (unless (memv (response-status-code resp) ok)
      (error who "request failed~n  status: ~s~n  body: ~e"
             (response-status-code resp)
             (response-body resp)))))
