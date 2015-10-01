#lang scribble/manual

@title{Envy: An environment variable manager}

@defmodule[envy #:lang]

All applications need some degree of configuration. Some options can be provided as command-line
flags, but often the configuration is too complex to be specified in this way. For all non-trivial
configuration, @link["http://12factor.net/config"]{applications should store configuration in the
environment} to stay organized and to remain portable.

Envy helps keep environment variables in order by creating a way to declaratively specify all of your
app's environment variables in one place, automatically wrap them in a module, parse them from raw
strings into a variety of different datatypes, and even properly associate them with Typed Racket
types.

@table-of-contents[]

@include-section["lang-envy.scrbl"]
@include-section["lang-envy-sexp.scrbl"]
@include-section["api-reference.scrbl"]