#lang s-exp syntax/module-reader envy/s-exp/lang/language

#:read sw:read
#:read-syntax sw:read-syntax

(require (submod sweet-exp link-reader)
         (prefix-in tr: typed-racket/typed-reader))

(define-values (sw:read sw:read-syntax)
  (sweet-link tr:read tr:read-syntax))
