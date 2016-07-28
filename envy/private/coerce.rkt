#lang typed/racket/base

;; This module implements typed coercion functions for converting strings to various datatypes.

(require threading)

(provide string->integer
         string->positive-integer
         string->negative-integer
         string->nonnegative-integer
         string->boolean)

;; Numeric Tower

(define-syntax define-string->number-coercion
  (syntax-rules (:)
    [(_ name : Type)
     (begin
       (: name (String -> Type))
       (define name (λ~> string->number
                         (cast Type))))]))

(define-string->number-coercion
  string->integer : Integer)

(define-string->number-coercion
  string->positive-integer : Positive-Integer)

(define-string->number-coercion
  string->negative-integer : Negative-Integer)

(define-string->number-coercion
  string->nonnegative-integer : Nonnegative-Integer)

;; Miscellaneous Types

(define (string->boolean [str : String]) : Boolean
  (case str
    [("true") #t]
    [("false") #f]
    [else (cast str Boolean)]))
