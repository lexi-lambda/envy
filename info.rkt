#lang info

(define name "envy")
(define pkg-desc "an environment variable manager")
(define version "1.0.0")

(define collection 'multi)

; #lang envy generates code that uses Typed Racket
(define implies
  '("typed-racket-lib"))

(define deps
  '("alexis-util"
    "base"
    "sweet-exp"
    "typed-racket-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "typed-racket-doc"
    "typed-racket-more"))
