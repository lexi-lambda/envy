#lang typed/racket/base

(require envy/environment
         typed/rackunit)

(require/typed
 racket/base
 [#:opaque Environment-Variables environment-variables?]
 [current-environment-variables (Parameterof Environment-Variables)]
 [make-environment-variables (->* () #:rest Bytes Environment-Variables)])


(parameterize ([current-environment-variables (make-environment-variables #"VAR_A" #"value-a"
                                                                          #"VAR_B" #"value-b")])
  (define-environment-variable var-a)
  (define-environment-variable var-b)
  (check-equal? var-a "value-a")
  (check-equal? var-b "value-b")
  
  (check-exn #rx"The required environment variable \"VAR_C\" is not defined."
             (lambda () (define-environment-variable var-c) (void))))

(parameterize ([current-environment-variables (make-environment-variables #"A_BOOLEAN" #"true"
                                                                          #"A_INTEGER" #"42")])
  (define-environment-variable a-boolean : Boolean)
  (define-environment-variable a-integer : Integer)
  (check-equal? a-boolean #t)
  (check-equal? a-integer 42))

(parameterize ([current-environment-variables (make-environment-variables #"AN_OCTAL_INTEGER" #"123")])
  (: octal-string->integer (-> String Integer))
  (define (octal-string->integer s)
    (cast (string->number s 8) Integer))
  (define-environment-variable an-octal-integer : Integer #:coerce octal-string->integer)
  (check-equal? an-octal-integer 83))
