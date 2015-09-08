#lang info

(define name "envy")
(define pkg-desc "an environment variable manager")
(define version "0.1.1")

(define collection 'multi)

(define deps
  '("alexis-util"
    "base"
    "typed-racket-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "typed-racket-doc"
    "typed-racket-more"))
