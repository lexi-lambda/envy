#lang typed-racket/minimal

(require (rename-in typed/racket/base
                    [#%module-begin tr:module-begin])
         envy
         (for-syntax racket/base))

(provide (except-out (all-from-out typed/racket/base)
                     tr:module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ clause ...)
     (with-syntax ([injected-require (datum->syntax stx '(require envy))])
       #'(tr:module-begin injected-require
                          (define/provide-environment clause ...)))]))
