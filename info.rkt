#lang info

(define name "envy")
(define pkg-desc "an environment variable manager")
(define version "0.1.0")

(define collection 'multi)

(define deps
  '("alexis-util"
    "base"
    "typed-racket-lib"
    "unstable-contract-lib"))
(define build-deps
  '("cover"
    "racket-doc"
    "scribble-lib"
    "typed-racket-doc"
    "typed-racket-more"))
