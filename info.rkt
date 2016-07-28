#lang info

(define name "envy")
(define pkg-desc "an environment variable manager")
(define version "1.0.0")

(define collection 'multi)

; #lang envy generates code that uses Typed Racket
(define implies
  '("typed-racket-lib"))

(define deps
  '("base"
    "sweet-exp-lib"
    "threading"
    "typed-racket-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "sweet-exp"
    "typed-racket-doc"
    "typed-racket-more"))
