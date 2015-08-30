#lang scribble/manual

@(require (for-label envy
                     typed/racket/base)
          racket/list
          scribble/eval)

@(define (make-sandbox)
   (let ([eval ((make-eval-factory '() #:lang 'typed/racket))])
     (eval '(require envy))
     eval))

@(define (make-environment-sandbox environment)
   (let ([eval (make-sandbox)]
         [make-env-variables-args (make-list (length environment) 'Bytes)])
     (eval `(require/typed
             racket/base
             [#:opaque Environment-Variables environment-variables?]
             [current-environment-variables (Parameterof Environment-Variables)]
             [make-environment-variables (,@make-env-variables-args -> Environment-Variables)]))
     (eval `(current-environment-variables (make-environment-variables ,@environment)))
     eval))

@title{Envy: An environment variable manager}

@defmodule[envy]

All applications need some degree of configuration. Some options can be provided as command-line
flags, but often the configuration is too complex to be specified in this way. For all non-trivial
configuration, @link["http://12factor.net/config"]{applications should store configuration in the
environment} to stay organized and to remain portable.

Envy helps keep environment variables in order by creating a way to declaratively specify all of your
app's environment variables in one place, automatically wrap them in a module, parse them from raw
strings into a variety of different datatypes, and even properly associate them with Typed Racket
types.

@section{Quickstart}

To get started, create a module to manage your application's environment variables (e.g.
@code{application.rkt}). Envy can be used by untyped @emph{or} typed Racket modules, but the manifest
module itself @emph{must} be written in Typed Racket.

@codeblock{
  #lang typed/racket
  (require envy)
}

To specify environment variables, use the @racket[define/provide-environment] form. Within the form,
include the names of the environment variables your application depends on.

@(racketblock
  (define/provide-environment
    some-environment-variable
    another-environment-variable))

Each entry in @racket[define/provide-environment] will produce a variable with the given name bound to
the value of the equivalent environment variable. The name of the environment variable will be
generated from the name of the Racket variable by converting the identifier to @tt{ALL_CAPS} and
converting dashes to underscores. In the above example, Envy would fetch the values for
@tt{SOME_ENVIRONMENT_VARIABLE} and @tt{ANOTHER_ENVIRONMENT_VARIABLE}.

When the module runs, the values for the specified variables will be loaded, but it's possible that
the variables don't actually exist in the environment. In this case, an error will be thrown.

@(interaction
  #:eval (make-sandbox)
  (define/provide-environment
    some-environment-variable))

If the environment variable @emph{does} exist, its value will be stored in the binding.

@(interaction
  #:eval (make-environment-sandbox '(#"SOME_ENVIRONMENT_VARIABLE" #"some value"))
  (eval:alts (define/provide-environment
               some-environment-variable
               another-environment-variable)
             (define-environment
               some-environment-variable))
  some-environment-variable)

To use the values of these environment variables in another module, just @racket[require] the module,
optionally with a prefix.

@(racketblock
  (require (prefix-in env: "environment.rkt")))

@subsection{Specifying types}

All environment variables are natively strings, but it is extremely common to store other kinds of
configuration data, such as booleans and numbers. Envy permits specifying a type with each variable,
and it will automatically parse the value to match the specified type.

For example, given the following environment:

@verbatim{
  HOST=racket-lang.org
  PARALLEL=true
  THREADS=42
}

...one could use the following environment definition:

@(interaction
  #:eval (make-environment-sandbox '(#"HOST" #"racket-lang.org"
                                     #"PARALLEL" #"true"
                                     #"THREADS" #"42"))
  (eval:alts (define/provide-environment
               [host : String]
               [parallel : Boolean]
               [threads : Positive-Integer])
             (define-environment
               [host : String]
               [parallel : Boolean]
               [threads : Positive-Integer]))
  host
  parallel
  threads)

Note that the values are defined with the specified types, useful for Typed Racket users. Also, since
@racket[String] is the default type, including it is not strictly necessary.

@subsection{Providing defaults}

Sometimes, configuration variables may be optional, in which case it is useful to provide a default
value instead of raising an error upon an undefined variable. This can be done with the
@racket[#:default] option.

@(interaction
  #:eval (make-sandbox)
  (eval:alts (define/provide-environment
               [optional-value #:default #f])
             (define-environment
               [optional-value #:default #f]))
  optional-value)

Note that the type is still properly preserved for Typed Racket users, so if the default value's type
is different from the type of the environment variable, the resulting type will be a union.

@subsection{Using explicit environment variable names}

Sometimes it is desired to define a variable with a different name from the name used in the
environment. This can be done with the @racket[#:name] option.

@(racketblock
  (define/provide-environment
    [custom-name #:name "ENVIRONMENT_NAME"]))

@section{Reference}

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
  name is inferred based on @racket[name-id]: the identifier is converted to all caps and all dashes
  are converted to underscores.

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
    @item{@racket[Nonpositive-Integer]}
    @item{@racket[Nonnegative-Integer]}]

  If the specified variable does not exist in the environment, @racket[name-id] is set to the value of
  @racket[default-expr]. If no @racket[default-expr] is provided, an error is raised.}

@defform[(define/provide-environment clause ...)]{
  Exactly the same as @racket[define-environment] but also @racket[provide]s each @racket[_name-id].}

@defform[(define-environment-variable name-id maybe-type option ...)]{
  Exactly the same as @racket[define-environment] but only defines a single variable. The syntax is
  the same as @racket[define-environment]'s @racket[_clause].}
