#lang scribble/manual

@(require (for-label envy
                     typed/racket/base))

@title[#:tag "syntax"]{Using Envy with S-expression syntax}

Envy's syntax does not look like traditional S-expressions, but in fact it is just using ordinary
Racket syntax with a special reader, called
@seclink["top" #:doc '(lib "sweet-exp/sweet.scrbl")]{“sweet expressions”}. The ordinary, S-expression
based syntax is exposed through the @racketmodname[envy/s-exp] language, as well as the
@racketmodname[envy] module, which can be used in any Typed Racket module via @racket[require].

@section{@tt{#lang envy/s-exp}}

@defmodule[envy/s-exp #:lang]

The @racketmodname[envy/s-exp] language works exactly like the @racketmodname[envy] language, but it
does not enable the sweet expression reader. Each declaration must be properly grouped.

@codeblock{
  #lang envy/s-exp
  [some-var : Positive-Integer #:default #f]
  [another-var : Boolean #:name "CUSTOM NAME"]
}

This language works just like using the @racketmodname[envy] module directly, but its body is wrapped
in @racket[define/provide-environment].

@section{The @racketmodname[envy] module}

Using @racketmodname[envy] as a module imports the Envy API, which allows embedding Envy's
functionality in larger modules.

@(racketblock
  (require @#,racketmodname[envy])
  (define/provide-environment
    [some-var : Positive-Integer #:default #f]
    [another-var : Boolean #:name "CUSTOM NAME"]))

For full information on all the forms provided by Envy, see the @secref["api-reference"].
