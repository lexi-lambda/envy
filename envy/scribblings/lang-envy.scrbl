#lang scribble/manual

@(require (for-label envy
                     typed/racket/base)
          scribble/eval
          "util.rkt")

@; ---------------------------------------------------------------------------------------------------

@title[#:tag "quickstart"]{Quickstart via @tt{#lang envy}}

To get started, create a module to manage your application's environment variables (e.g.
@code{environment.rkt}).

@codeblock{
  #lang envy
}

To specify which environment variables your application depends on, specify each variable's name on a
separate line:

@codeblock{
  #lang envy
  some-environment-variable
  another-environment-variable
}

Each line of your environment manifest will produce a variable with the given name bound to the value
of the equivalent environment variable. The name of the environment variable will be generated from
the name of the Racket variable by converting the identifier to @tt{ALL_CAPS}, converting dashes to
underscores, and stripping question marks. In the above example, Envy would fetch the values for
@tt{SOME_ENVIRONMENT_VARIABLE} and @tt{ANOTHER_ENVIRONMENT_VARIABLE}.

When the module runs, the values for the specified variables will be loaded, but it's possible that
the variables don't actually exist in the environment. In this case, an error will be thrown.

@codeblock{
  #lang envy
  some-environment-variable
}

@(interaction
  #:eval (make-sandbox)
  (eval:alts code:blank
             (define-environment
               some-environment-variable)))

If the environment variable @emph{does} exist, its value will be stored in the binding.

@codeblock{
  #lang envy
  some-environment-variable
  another-environment-variable
}

@(interaction
  #:eval (make-environment-sandbox '(#"SOME_ENVIRONMENT_VARIABLE" #"some value"))
  (eval:alts some-environment-variable
             (begin (define-environment
                      some-environment-variable)
                    some-environment-variable)))

To use the values of these environment variables in another module, just @racket[require] the module,
optionally with a prefix.

@(racketblock
  (require (prefix-in env: "environment.rkt")))

@; ---------------------------------------------------------------------------------------------------

@section{Specifying types}

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

@codeblock{
  host : String
  parallel? : Boolean
  threads : Positive-Integer
}

@(interaction
  #:eval (make-environment-sandbox '(#"HOST" #"racket-lang.org"
                                     #"PARALLEL" #"true"
                                     #"THREADS" #"42"))
  (eval:alts host
             (begin (define-environment
                      [host : String]
                      [parallel? : Boolean]
                      [threads : Positive-Integer])
                    host))
  parallel?
  threads)

Note that the values are defined with the specified types, useful for Typed Racket users. Also, since
@racket[String] is the default type, including it is not strictly necessary.

@; ---------------------------------------------------------------------------------------------------

@section{Providing defaults}

Sometimes, configuration variables may be optional, in which case it is useful to provide a default
value instead of raising an error upon an undefined variable. This can be done with the
@racket[#:default] option.

@codeblock{
  optional-value #:default #f
}

@(interaction
  #:eval (make-sandbox)
  (eval:alts optional-value
             (begin
               (define-environment
                 [optional-value #:default #f])
               optional-value)))

Note that the type is still properly preserved for Typed Racket users, so if the default value's type
is different from the type of the environment variable, the resulting type will be a union.

@; ---------------------------------------------------------------------------------------------------

@section{Using explicit environment variable names}

Sometimes it is desired to define a variable with a different name from the name used in the
environment. This can be done with the @racket[#:name] option.

@codeblock{
  custom-name #:name "ENVIRONMENT_NAME"
}
