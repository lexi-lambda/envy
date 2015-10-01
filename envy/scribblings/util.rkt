#lang racket/base

(require racket/list
         scribble/eval)

(provide make-sandbox make-environment-sandbox)

(define (make-sandbox)
  (let ([eval ((make-eval-factory '() #:lang 'typed/racket))])
    (eval '(require envy))
    eval))

(define (make-environment-sandbox environment)
  (let ([eval (make-sandbox)]
        [make-env-variables-args (make-list (length environment) 'Bytes)])
    (eval `(require/typed
            racket/base
            [#:opaque Environment-Variables environment-variables?]
            [current-environment-variables (Parameterof Environment-Variables)]
            [make-environment-variables (,@make-env-variables-args -> Environment-Variables)]))
    (eval `(current-environment-variables (make-environment-variables ,@environment)))
    eval))
