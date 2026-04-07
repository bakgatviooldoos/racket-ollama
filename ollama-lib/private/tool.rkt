#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         json
         json/to-jsexpr
         racket/generic
         racket/port
         struct-define
         threading
         "json-schema.rkt")

(provide
 (all-from-out "json-schema.rkt")
 (struct-out tool-arg-info)
 (struct-out tool-info)

 :
 Object
 define-tool-definer

 raise-tool-error
 (struct-out exn:fail:tool)
 (struct-out exn:fail:tool:not-found)
 (struct-out exn:fail:tool:call))

(define current-call-data
  (make-parameter #f))

(define validate-tool-arg-schema?
  (make-parameter #f))

(struct exn:fail:tool exn:fail (data hints)
  #:methods gen:to-jsexpr
  [(define (->jsexpr e)
     (struct-define exn:fail:tool e)
     (~> data
         (hash-set 'error (exn-message e))
         (hash-set 'hints hints)))])
(struct exn:fail:tool:not-found exn:fail:tool ())
(struct exn:fail:tool:call exn:fail:tool ())

(define (raise-tool-error
         #:hints [hints null]
         #:data [data (current-call-data)]
         message . args)
  (raise
   (exn:fail:tool:call
    (apply format message args)
    (current-continuation-marks)
    #;data data
    #;hints hints)))

(define-syntax (: stx)
  (raise-syntax-error ': "may only be used within a define-tool or an Object form" stx))

(define-syntax (Object stx)
  (syntax-parse stx
    #:literals (:)
    [(_ [k : t] ...+)
     #'(Object* (list (cons 'k t) ...))]))

(struct tool-arg-info (id label type))
(struct tool-info (id description examples proc args)
  #:methods gen:to-jsexpr
  [(define (->jsexpr t)
     (struct-define tool-info t)
     (hasheq
      'type "function"
      'examples (for/list ([example (in-list examples)])
                  (cond
                    [(string? example) example]
                    [else (call-with-output-string
                           (lambda (out)
                             (write example out)))]))
      'description description
      'function
      (hasheq
       'name (symbol->string id)
       'parameters
       (Object*
        (for/list ([arg (in-list args)])
          (struct-define tool-arg-info arg)
          (cons label type))))))])

(begin-for-syntax
  (define-syntax-class arg
    #:literals (:)
    (pattern [id:id : type:expr] #:attr label #'id)
    (pattern [label:id id:id : type:expr])))

(define-syntax (define-tool-definer stx)
  (syntax-parse stx
    [(_ definer:id #:getter get-tools:id #:caller call-tool:id)
     #'(begin
         (define tools (make-hasheq))
         (define-syntax (definer stx)
           (syntax-parse stx
             [(_ (id:id arg:arg (... ...))
                 {~optional {~seq #:description description:expr}}
                 {~seq #:example example:expr} (... ...)
                 e ...+)
              #'(begin
                  (define (id arg.id (... ...)) e (... ...))
                  (define info
                    (tool-info
                     #;id 'id
                     #;description (... {~? description "No description."})
                     #;examples (... {~? (list example ...) ""})
                     #;proc id
                     #;args (list
                             (tool-arg-info
                              #;id 'arg.id
                              #;label 'arg.label
                              #;type arg.type) (... ...))))
                  (hash-set! tools 'id info))]))
         (define (get-tools)
           (hash-copy tools))
         (define (call-tool data #:validate? [validate? (validate-tool-arg-schema?)])
           (do-call-tool tools data validate?)))]))

(define (do-call-tool tools data validate?)
  (define func (hash-ref data 'function))
  (define name-str (hash-ref func 'name))
  (define name (string->symbol name-str))
  (define arguments (hash-ref func 'arguments))
  (define info
    (hash-ref
     #;ht tools
     #;key name
     #;failure-result
     (lambda ()
       (raise
        (exn:fail:tool:not-found
         (format "tool '~a' does not exist" name)
         (current-continuation-marks)
         #;data data
         #;hints '("check the tool list again and retry"))))))
  (struct-define tool-info info)
  (define arg-vals
    (for/list ([arg (in-list args)])
      (struct-define tool-arg-info arg)
      (define value
        (hash-ref
         #;ht arguments
         #;key label
         #;failure-result
         (lambda ()
           (raise-tool-error
            #:hints '("check the tool's schema again and retry")
            #:data data
            "tool '~a' requires '~a' as an argument" name label))))
      (when validate?
        (unless (json-is? value type)
          (raise-tool-error
           #:hints '("check the argument's schema again and retry")
           #:data data
           "invalid argument in tool '~a':~nargument '~a' expected type '~a', received: '~a'"
           name label
           (jsexpr->string type)
           (jsexpr->string value))))
      value))
  (define res
    (parameterize ([current-call-data data])
      (apply proc arg-vals)))
  (jsexpr->string
   (hash-set data 'result (->jsexpr res))))
