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
 make-ollama-options
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
         #:logprobs? [logprobs? (json-null)]
         #:top-logprobs [top-logprobs (json-null)]
         #:response-> [response-> void]
         c model [user-prompt (json-null)])
  (struct-define ollama-client c)
  (define resp
    (~> (session-request
         #:method 'post
         #:stream? #t
         #:auth auth
         #:json ((json-options)
                 'stream #t
                 'model model
                 'prompt user-prompt
                 'system system-prompt
                 'suffix suffix
                 'think (.? think? ->jsexpr)
                 'format (.? output-format ->jsexpr)
                 'images (map bytes->string/utf-8 images)
                 'raw raw?
                 'keep_alive keep-alive
                 'options options
                 'logprobs logprobs?
                 'top_logprobs top-logprobs)
         #:timeouts (ollama-timeouts)
         session (~endpoint "api" "generate"))
        (check-response 'ollama-generate)
        (->producer)))
  (let ([parts (mutable-treelist)])
    (lambda ()
      (define data (resp))
      (begin0 data
        (if (eof-object? data)
            (response->
             (parts->complete-response parts))
            (mutable-treelist-add! parts data))))))

;; IMAGES (EXPERIMENTAL)
(define (ollama-generate-image
         #:width width
         #:height height
         #:steps [steps (json-null)]
         c model user-prompt)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:stream? #t
       #:auth auth
       #:json ((json-options)
               'stream #t
               'model model
               'prompt user-prompt
               'width width
               'height height
               'steps steps)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "generate"))
      (check-response 'ollama-generate-image)
      (->producer)))

;; CHAT
(define (ollama-start-chat
         #:options [options (json-null)]
         #:format [output-format (json-null)]
         #:think? [think? (json-null)]
         #:tools [tools (json-null)]
         #:keep-alive [keep-alive (json-null)]
         #:logprobs? [logprobs? (json-null)]
         #:top-logprobs [top-logprobs (json-null)]
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
                   'stream #t
                   'model model
                   'options options
                   'keep_alive keep-alive
                   'messages (->jsexpr messages)
                   'tools (.? tools ->jsexpr/hash-values)
                   'think (.? think? ->jsexpr)
                   'format (.? output-format ->jsexpr)
                   'logprobs logprobs?
                   'top_logprobs top-logprobs)
           #:timeouts (ollama-timeouts)
           session (~endpoint "api" "chat"))
          (check-response 'ollama-chat)
          (->producer)))
    (let ([messages (treelist-copy messages)]
          [parts (mutable-treelist)])
      (values
       (lambda ()
         (define data (resp))
         (begin0 data
           (cond
             [(eof-object? data)
              (define complete-message
                (parts->complete-message parts))
              (and~>>
               (&message.content complete-message)
               (non-empty-string?)
               (and _ complete-message)
               (response->history-entry)
               (mutable-treelist-add! messages))]
             [else
              (mutable-treelist-add! parts data)])))
       (lambda (#:format [output-format output-format] ;; noqa
                #:tools [tools tools] ;; noqa
                next-message)
         (if (list? next-message)
             (mutable-treelist-append! messages (ensure-messages next-message))
             (mutable-treelist-add! messages (ensure-message next-message)))
         (loop (mutable-treelist-snapshot messages) output-format tools))))))

(define (make-ollama-options
         #:seed [seed (json-null)]
         #:temperature [temperature (json-null)]
         #:top-k [top-k (json-null)]
         #:top-p [top-p (json-null)]
         #:min-p [min-p (json-null)]
         #:stop [stop (json-null)]
         #:num-ctx [num-ctx (json-null)]
         #:num-predict [num-predict (json-null)])
  ((json-options)
   'seed seed
   'temperature temperature
   'top_k top-k
   'top_p top-p
   'min_p min-p
   'stop stop
   'num_ctx num-ctx
   'num_predict num-predict))

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
      (check-response 'ollama-embed)
      (response-json)))

;; MODELS
(define (ollama-list-models c)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'get
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "tags"))
      (check-response 'ollama-list-models)
      (response-json)))

(define (ollama-list-running c)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'get
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "ps"))
      (check-response 'ollama-list-running)
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
      (check-response 'ollama-show-model)
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
         #:quantize [quantize (json-null)]
         c model)
  (struct-define ollama-client c)
  (let ([messages (ensure-messages messages)])
    (~> (session-request
         #:method 'post
         #:stream? #t
         #:auth auth
         #:json ((json-options)
                 'stream #t
                 'model model
                 'from from
                 'files files
                 'adapters adapters
                 'template template
                 'license license
                 'system system
                 'parameters parameters
                 'messages (.? messages ->jsexpr)
                 'quantize (.? quantize ->jsexpr))
         #:timeouts (ollama-timeouts)
         session (~endpoint "api" "create"))
        (check-response 'ollama-create-model)
        (->producer))))

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
      (check-response 'ollama-copy-model)
      (void)))

(define (ollama-pull-model
         #:insecure? [insecure? (json-null)]
         c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:stream? #t
       #:auth auth
       #:json ((json-options)
               'stream #t
               'model model
               'insecure insecure?)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "pull"))
      (check-response 'ollama-pull-model)
      (->producer)))

(define (ollama-push-model
         #:insecure? [insecure? (json-null)]
         c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'post
       #:stream? #t
       #:auth auth
       #:json ((json-options)
               'stream #t
               'model model
               'insecure insecure?)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "push"))
      (check-response 'ollama-push-model)
      (->producer)))

(define (ollama-delete-model c model)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'delete
       #:auth auth
       #:json (hasheq 'model model)
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "delete"))
      (check-response 'ollama-delete-model)
      (void)))

;; VERSION
(define (ollama-version c)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'get
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "version"))
      (check-response 'ollama-version)
      (response-json)))

;; FILE-BLOBS
(define (ollama-has-blob? c sha256)
  (struct-define ollama-client c)
  (~> (session-request
       #:method 'head
       #:auth auth
       #:timeouts (ollama-timeouts)
       session (~endpoint "api" "blobs" (~sha256 sha256)))
      (check-response 'ollama-has-blob? '(200 404))
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
         session (~endpoint "api" "blobs" (~sha256 sha256)))
        (check-response 'ollama-upload-blob '(201))
        (and sha256))))

;; JSON
(define (->jsexpr/hash-values hash)
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
(define (check-response resp who [ok '(200)])
  (begin0 resp
    (unless (memv (response-status-code resp) ok)
      (error who "request failed~n  status: ~s~n  body: ~e"
             (response-status-code resp)
             (response-body resp)))))

(define (->producer resp)
  (let ([inp (response-output resp)])
    (lambda ()
      (cond
        [(port-closed? inp) eof]
        [else
         (define data (read-json inp))
         (begin0 data
           (when (eof-object? data)
             (response-close! resp)))]))))

;; FILE-BLOB HELPERS
(define (~sha256 hash) (format "sha256:~a" hash))

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
