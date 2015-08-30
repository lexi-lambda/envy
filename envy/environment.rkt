#lang typed/racket/base

(require (for-syntax alexis/util/threading
                     racket/base
                     racket/dict
                     racket/string
                     racket/syntax
                     syntax/id-table)
         syntax/parse/define
         "private/coerce.rkt")

(provide define-environment
         define/provide-environment
         define-environment-variable)

(define-for-syntax auto-type-table
  (make-immutable-free-id-table
   `((,#'String              . ,#'(inst values String))
     (,#'Symbol              . ,#'string->symbol)
     (,#'Boolean             . ,#'string->boolean)
     (,#'Number              . ,#'string->number)
     (,#'Integer             . ,#'string->integer)
     (,#'Positive-Integer    . ,#'string->positive-integer)
     (,#'Negative-Integer    . ,#'string->negative-integer)
     (,#'Nonpositive-Integer . ,#'string->nonpositive-integer)
     (,#'Nonnegative-Integer . ,#'string->nonnegative-integer))))

(define-syntax-parser define-environment-variable
  #:literals (:)
  [(_ (~describe "name" name:id)
      (~optional (~describe "type" (~seq : type:id)) #:defaults ([type #'String]))
      (~or (~optional (~seq #:name env-var-name:expr) #:defaults ([env-var-name #f]))
           (~optional (~seq #:default default:expr) #:defaults ([default #f])))
      ...)
   (with-syntax* ([env-var-name (or (attribute env-var-name)
                                    (~> (syntax-e #'name)
                                        symbol->string
                                        string-upcase
                                        (string-replace "-" "_")))]
                  [coerce (dict-ref auto-type-table #'type)]
                  [fetch-env-var
                   (if (attribute default)
                       #'(require-environment-variable env-var-name coerce default)
                       #'(require-environment-variable env-var-name coerce))])
     #'(define name fetch-env-var))])

(begin-for-syntax
  (define-syntax-class environment-clause
    #:attributes (name normalized)
    (pattern name:id #:with normalized #'[name])
    (pattern [name:id args ...] #:with normalized #'[name args ...])))

(define-syntax-parser define-environment
  [(_ clause:environment-clause ...)
   (with-syntax ([((normalized ...) ...) #'(clause.normalized ...)])
     #'(begin (define-environment-variable normalized ...) ...))])

(define-syntax-parser define/provide-environment
  [(_ clause:environment-clause ...)
   (with-syntax ([(name ...) #'(clause.name ...)]
                 [((normalized ...) ...) #'(clause.normalized ...)])
     #'(begin (begin (define-environment-variable normalized ...)
                     (provide name))
              ...))])

(: require-environment-variable (All [a b] (case-> (String (String -> a) -> a)
                                                   (String (String -> a) b -> (U a b)))))
(define require-environment-variable
  (case-lambda
    [(name parse)
     (let ([value (getenv name)])
       (unless value
         (error 'require-environment-variable
                "The required environment variable \"~a\" is not defined." name))
       (parse value))]
    [(name parse default)
     (let ([value (getenv name)])
       (if value (parse value) default))]))
