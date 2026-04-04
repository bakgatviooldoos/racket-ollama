#lang racket/base

(require json
         net/http-easy
         racket/contract/base
         "private/client.rkt"
         "private/json.rkt"
         "private/json-schema.rkt"
         "private/message.rkt"
         "private/sha.rkt"
         "private/tool.rkt")

(provide
 ollama-client?
 (contract-out
  [ollama-timeouts
   (parameter/c timeout-config?)]
  
  [make-ollama-client
   (->* []
        [string?
         #:auth auth-procedure/c]
        ollama-client?)]
  [ollama-generate
   (->* [ollama-client? string?]
        [#:suffix (or-json-null/c string?)
         #:images (listof bytes?)
         #:format (or-json-null/c 'json jsexpr?)
         #:system (or-json-null/c string?)
         #:think? (or-json-null/c boolean? 'low 'medium 'high)
         #:raw? (or-json-null/c boolean?)
         #:keep-alive (or-json-null/c string? seconds/c)
         #:options (or-json-null/c jsexpr-hash/c)
         #:response->message (-> jsexpr? (or/c #f message?))
         #:message-callback (-> message? void?)
         (or-json-null/c string?)]
        chat-response/c)]
  [ollama-start-chat
   (->* [ollama-client? string? chat-message*/c]
        [#:options (or-json-null/c jsexpr-hash/c)
         #:format (or-json-null/c 'json jsexpr?)
         #:think? (or-json-null/c boolean? 'low 'medium 'high)
         #:tools (or-json-null/c (hash/c symbol? tool-info?))
         #:response->history-entry (-> jsexpr? (or/c #f message?))]
        (values
         chat-response/c
         chat-continuation/c))]

  [ollama-embed
   (->* [ollama-client? string? (or/c string? (listof string?))]
        [#:truncate? (or-json-null/c boolean?)
         #:dimensions (or-json-null/c natural-number/c)
         #:options (or-json-null/c jsexpr-hash/c)
         #:keep-alive (or-json-null/c string? seconds/c)]
        jsexpr?)]
  
  [ollama-list-models
   (-> ollama-client?
       jsexpr?)]
  [ollama-list-running
   (-> ollama-client?
       jsexpr?)]
  [ollama-show-model
   (->* [ollama-client? string?]
        [#:verbose? (or-json-null/c boolean?)]
        jsexpr?)]

  [ollama-load-model
   (-> ollama-client? string?
       any)]
  [ollama-unload-model
   (-> ollama-client? string?
       any)]

  [ollama-create-model
   (->* [ollama-client? string?]
        [#:from (or-json-null/c string?)
         #:files (or-json-null/c file->blob/c)
         #:adapters (or-json-null/c file->blob/c)
         #:template (or-json-null/c string?)
         #:license (or-json-null/c string? (listof string?))
         #:system (or-json-null/c string?)
         #:parameters (or-json-null/c jsexpr-hash/c)
         #:messages (or-json-null/c chat-message*/c)
         #:stream? boolean?
         #:quantize (or-json-null/c quantize/c)]
        (or/c jsexpr? response-stream/c))]
  [ollama-copy-model
   (-> ollama-client? string? string?
       any)]
  [ollama-pull-model
   (->* [ollama-client? string?]
        [#:insecure? (or-json-null/c boolean?)
         #:stream? boolean?]
        (or/c jsexpr? response-stream/c))]
  [ollama-push-model
   (->* [ollama-client? string?]
        [#:insecure? (or-json-null/c boolean?)
         #:stream? boolean?]
        (or/c jsexpr? response-stream/c))]
  [ollama-delete-model
   (-> ollama-client? string?
       any)]
  
  [ollama-has-blob?
   (-> ollama-client? sha256-string?
       boolean?)]
  [ollama-upload-blob
   (->* [ollama-client? blob-data/c]
        [#:sha256 (or/c #f sha256-string?)]
        sha256-string?)]
  
  [ollama-version
   (-> ollama-client?
       jsexpr?)])

 message?
 (contract-out
  [make-message
   (->* [string?]
        [#:role (or/c 'assistant 'system 'user 'tool)
         #:images (listof bytes?)]
        message?)])

 chat-response/c
 chat-continuation/c

 :
 tool-info?
 define-tool-definer
 (all-from-out "private/json-schema.rkt")

 exn:fail:tool?
 exn:fail:tool:not-found?
 exn:fail:tool:call?
 raise-tool-error)

(define jsexpr-hash/c
  (hash/c symbol? jsexpr?))

(define (or-json-null/c . cs)
  (apply or/c json-null? cs))

(define seconds/c natural-number/c)

(define file->blob/c
  (hash/c symbol? sha256-string?))

(define chat-message*/c
  (or/c string? message? (listof (or/c string? message?))))

(define response-stream/c
  (-> (or/c jsexpr? eof-object?)))

(define chat-response/c response-stream/c)

(define chat-continuation/c
  (->* [chat-message*/c]
       [#:format (or-json-null/c 'json jsexpr?)
        #:tools (or-json-null/c (hash/c symbol? tool-info?))]
       (values
        chat-response/c
        (recursive-contract chat-continuation/c))))

;; recommended: q4_K_M, q8_0
(define quantize/c
  (or/c 'q2_K
        'q3_K_L 'q3_K_M 'q3_K_S
        'q4_0 'q4_1 'q4_K_M 'q4_K_S
        'q5_0 'q5_1 'q5_K_M 'q5_K_S
        'q6_K
        'q8_0))

(define blob-data/c
  (or/c bytes? string? path? input-port?))
