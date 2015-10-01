#lang scribble/manual

@(require (for-label envy
                     typed/racket/base))

@title[#:tag "api-reference"]{API Reference}

@defform[#:literals (:)
         (define-environment clause ...)
         #:grammar
         ([clause name-id
                  [name-id maybe-type option ...]]
          [maybe-type (code:line)
                      (code:line : type-id)]
          [option (code:line #:name env-var-name-expr)
                  (code:line #:default default-expr)])]{
  Defines a set of variables to be initialized with values from the environment.

  Each @racket[name-id] is assigned the value of the environment variable with the name
  @racket[env-var-name-expr]. If no @racket[env-var-name-expr] is provided, the environment variable
  name is inferred based on @racket[name-id]: the identifier is converted to all caps, all dashes are
  converted to underscores, and all question marks are stripped.

  Before being assigned to @racket[name-id], the value of the environment variable is parsed based on
  @racket[type-id]. If no @racket[type-id] is provided, the type is inferred to be @racket[String].
  The following types are supported:

  @itemlist[
    @item{@racket[String]}
    @item{@racket[Symbol]}
    @item{@racket[Boolean] (must be either @racket["true"] or @racket["false"])}
    @item{@racket[Number]}
    @item{@racket[Integer]}
    @item{@racket[Positive-Integer]}
    @item{@racket[Negative-Integer]}
    @item{@racket[Nonnegative-Integer]}]

  If the specified variable does not exist in the environment, @racket[name-id] is set to the value of
  @racket[default-expr]. If no @racket[default-expr] is provided, an error is raised.}

@defform[(define/provide-environment clause ...)]{
  Exactly the same as @racket[define-environment] but also @racket[provide]s each @racket[_name-id].}

@defform[(define-environment-variable name-id maybe-type option ...)]{
  Exactly the same as @racket[define-environment] but only defines a single variable. The syntax is
  the same as @racket[define-environment]'s @racket[_clause].}

