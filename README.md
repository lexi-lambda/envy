# Envy [![Build Status](https://travis-ci.org/lexi-lambda/envy.svg?branch=master)](https://travis-ci.org/lexi-lambda/envy)

Envy is an environment variable manager for Racket applications.

  - Specify your environment variables in a declarative manifest, then use them as plain Racket variables.
  - Automatically fail with helpful error messages when required environment variables are not present.
  - Include types on your variables to automatically parse string values into Racket datatypes.

Envy supports plain Racket and Typed Racket out of the box; just install the `envy` package and go!

For information on how to get started, [take a look at the documentation][docs].

## Credits

Name and functionality inspired by [Envied][envied] for Ruby.

[docs]: https://lexi-lambda.github.io/envy/envy.html
[envied]: https://github.com/eval/envied
