#lang racket/base

(require json
         json/to-jsexpr
         net/http-easy
         racket/mutable-treelist
         racket/port
         racket/string
         racket/treelist
         struct-define
         threading
         "json.rkt"
         "lens.rkt"
         "message.rkt"
         "sha.rkt"
         "tool.rkt")

(provide
 make-ollama-client
 ollama-client?
 ollama-timeouts
 ollama-generate
 ollama-start-chat
 ollama-embed
 ollama-list-models
 ollama-list-running
 ollama-load-model
 ollama-unload-model
 ollama-show-model
 ollama-create-model
 ollama-copy-model
 ollama-pull-model
 ollama-push-model
 ollama-delete-model
 ollama-has-blob?
 ollama-upload-blob
 ollama-version)

(define ollama-timeouts
  (make-parameter
   (make-timeout-config
    #:request (* 5 60))))

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
         #:images [images null]
         #:format [output-format (json-null)]
         #:system [system-prompt (json-null)]
         #:think? [think? (json-null)]
         #:raw? [raw? (json-null)]
         #:keep-alive [keep-alive (json-null)]
         #:options [options (json-null)]
         #:response-> [response-> void]
         c model [user-prompt (json-null)])
  (struct-define ollama-client c)
  (define resp
    (~> (session-request
         #:method 'post
         #:stream? #t
         #:auth auth
         #:json ((json-options)
                 'model model
                 'stream #t
                 'prompt user-prompt
                 'suffix suffix
                 'images (map bytes->string/utf-8 images)
                 'system system-prompt
                 'options options
                 'think (.? think? ->jsexpr)
                 'raw raw?
                 'keep_alive keep-alive
                 'format (.? output-format ->jsexpr))
         #:timeouts (ollama-timeouts)
         session (~endpoint "api" "generate"))
        (check-response 'ollama-generate _)))
  (let ([parts (mutable-treelist)]
        [inp (response-output resp)])
    (lambda ()
      (cond
        [(port-closed? inp) eof]
        [else
         (define data (read-json inp))
         (begin0 data
           (cond
             [(eof-object? data)
              (response->
               (parts->complete-message parts))
              (response-close! resp)]
             [else
              (mutable-treelist-add! parts data)]))]))))

;; IMAGES (EXPERIMENTAL)
(define (ollama-generate-image
         #:width width
         #:height height
         #:steps [steps (json-null)]
         #:stream? [stream? #t]
         c model user-prompt)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:stream? #t
       #:auth auth
       #:json ((json-options)
               'model model
               'stream #t
               'prompt user-prompt
               'width width
               'height height
               'steps steps)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "generate"))
      (check-response 'ollama-generate-image _)
      (stream-when stream?)))

;; CHAT
(define (ollama-start-chat
         #:options [options (json-null)]
         #:format [output-format (json-null)]
         #:think? [think? (json-null)]
         #:tools [tools (json-null)]
         #:response->history-entry
         [response->history-entry
          (lambda (data)
            (make-message
             #:role 'assistant
             (&message.content data)))]
         c model str-or-messages)
  (struct-define ollama-client c)
  (let loop ([messages (ensure-messages str-or-messages)]
             [output-format output-format]
             [tools tools])
    (define resp
      (~> (session-request
           #:method 'post
           #:stream? #t
           #:auth auth
           #:json ((json-options)
                   'model model
                   'stream #t
                   'options options
                   'messages (->jsexpr messages)
                   'think (.? think? ->jsexpr)
                   'tools (.? tools hash-values->jsexpr)
                   'format (.? output-format ->jsexpr))
           #:timeouts (ollama-timeouts)
           session (~endpoint "api" "chat"))
          (check-response 'ollama-chat _)))
    (let ([messages (treelist-copy messages)]
          [parts (mutable-treelist)]
          [inp (response-output resp)])
      (values
       (lambda ()
         (cond
           [(port-closed? inp) eof]
           [else
            (define data (read-json inp))
            (begin0 data
              (cond
                [(eof-object? data)
                 (define complete-message
                   (parts->complete-message parts))
                 (and~>
                  (&message.content complete-message)
                  (non-empty-string? _)
                  (and _ complete-message)
                  (response->history-entry _)
                  (mutable-treelist-add! messages _))
                 (response-close! resp)]
                [else
                 (mutable-treelist-add! parts data)]))]))
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
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:auth auth
       #:json ((json-options)
               'model model
               'input input
               'dimensions dimensions
               'truncate truncate?
               'options options
               'keep_alive keep-alive)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "embed"))
      (check-response 'ollama-embed _)
      (response-json)))

;; MODELS
(define (ollama-list-models c)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'get
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "tags"))
      (check-response 'ollama-list-models _)
      (response-json)))

(define (ollama-list-running c)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'get
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "ps"))
      (check-response 'ollama-list-running _)
      (response-json)))

(define (ollama-load-model client model)
  (~> (ollama-generate client model)
      (void)))

(define (ollama-unload-model client model)
  (~> (ollama-generate client model #:keep-alive 0)
      (void)))

(define (ollama-show-model
         #:verbose? [verbose? (json-null)]
         c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:auth auth
       #:json ((json-options)
               'model model
               'verbose verbose?)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "show"))
      (check-response 'ollama-show-model _)
      (response-json)))

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
  (struct-define ollama-client c)
  (let ([messages (ensure-messages messages)])
    (~> (session-request
         #:method 'post
         #:stream? #t
         #:auth auth
         #:json ((json-options)
                 'model model
                 'from from
                 'files files
                 'stream stream?
                 'adapters adapters
                 'template template
                 'license license
                 'system system
                 'parameters parameters
                 'messages (.? messages ->jsexpr)
                 'quantize (.? quantize ->jsexpr))
         #:timeouts (ollama-timeouts)
         session (~endpoint "api" "create"))
        (check-response 'ollama-create-model _)
        (stream-when stream?))))

(define (ollama-copy-model c model destination)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:auth auth
       #:json (hasheq
               'source model
               'destination destination)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "copy"))
      (check-response 'ollama-copy-model _)
      (void)))

(define (ollama-pull-model
         #:insecure? [insecure? (json-null)]
         #:stream? [stream? #t]
         c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:stream? #t
       #:auth auth
       #:json ((json-options)
               'model model
               'insecure insecure?
               'stream stream?)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "pull"))
      (check-response 'ollama-pull-model _)
      (stream-when stream?)))

(define (ollama-push-model
         #:insecure? [insecure? (json-null)]
         #:stream? [stream? #t]
         c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:stream? #t
       #:auth auth
       #:json ((json-options)
               'model model
               'insecure insecure?
               'stream stream?)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "push"))
      (check-response 'ollama-push-model _)
      (stream-when stream?)))

(define (ollama-delete-model c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'delete
       #:auth auth
       #:json (hasheq 'model model)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "delete"))
      (check-response 'ollama-delete-model _)
      (void)))

;; VERSION
(define (ollama-version c)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'get
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "version"))
      (check-response 'ollama-version _)
      (response-json)))

;; FILE-BLOBS
(define (ollama-has-blob? c sha256)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'head
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "blobs" (format "sha256:~a" sha256)))
      (check-response 'ollama-has-blob? _ '(200 404))
      (response-status-code)
      (= 200)))

(define (ollama-upload-blob
         #:sha256 [sha256 #f]
         c data)
  (struct-define ollama-client c)
  (let ([sha256 (or sha256 (blob-sha256 data))])
    (~> (session-request
         #:method 'post
         #:auth auth
         #:data (->port data)
         #:timeouts (ollama-timeouts)
         session (~endpoint "api" "blobs" (format "sha256:~a" sha256)))
        (check-response 'ollama-upload-blob _ '(201))
        (and sha256))))

;; JSON
(define (hash-values->jsexpr hash)
  (->jsexpr (hash-values hash)))

;; MESSAGES
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

;; RESPONSES
(define (check-response who resp [ok '(200)])
  (begin0 resp
    (unless (memv (response-status-code resp) ok)
      (error who "request failed~n  status: ~s~n  body: ~e"
             (response-status-code resp)
             (response-body resp)))))

(define (stream-when resp stream?)
  (if (not stream?)
      (response-json resp)
      (let ([inp (response-output resp)])
        (lambda ()
          (cond
            [(port-closed? inp) eof]
            [else
             (define data (read-json inp))
             (begin0 data
               (when (eof-object? data)
                 (response-close! resp)))])))))

;; FILE-BLOB HELPERS
(define (->port data [dup? #f])
  (cond
    [(bytes? data)
     (open-input-bytes data)]
    [(string? data)
     (open-input-string data)]
    [(path? data)
     (open-input-file data)]
    [(input-port? data)
     (if (not dup?) data (dup-input-port data))]))

(define (blob-sha256 data)
  (bytes->hex-string
   (sha256-bytes
    (->port data #;dup? #t))))
