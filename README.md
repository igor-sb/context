# context

<!-- badges: start -->

[![R-CMD-check](https://github.com/igor-sb/context/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/igor-sb/context/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/igor-sb/context/branch/main/graph/badge.svg)](https://app.codecov.io/gh/igor-sb/context?branch=main)

<!-- badges: end -->

Context manager is used to create a context: a code block that runs in the
current R environment, where another object is temporarily created and safely
destroyed after the code block finishes running or throws an error. A common use
is to open a file, do stuff with it, then close the file connection. For
example:

``` r
library(context)

with(open(filename) %as% f, {
  x <- do_stuff(f)
})

do_stuff2(x)
```

is equivalent to this Python code:

``` py
with open(filename) as f:
  x = do_stuff(f)
  
do_stuff2(x)
```

## Implementation

ContextManager S3 object is created using `create_context_manager()` function,
which takes 3 arguments:

-   `on_enter`: function to evaluate before code block. Return value of this
    function is stored in the variable on the right side of the `%as%` operator.

-   `on_exit`: function to evaluate after code block completes or stops.

-   `args`: arguments passed on the `on_enter` function.

This can be use to create custom context managers.

## Alternative `with`s

Base R function `base::with` functions differently: it creates a new environment
and has no support for setup and teardown code. Even its own documentation
discourages its use (!).

`context::with` is much more similiar to `withr::with_*` functions from the
[withr package](https://withr.r-lib.org/) in that they both have the setup and
teardown code. However, `withr::with_*` functions run the code block in their
own environment and have a somewhat different purpose, which is to run code
under modified global state. I find them clunky and inflexible to use for this
purpose. For example, to modify a current environment variable, one needs even
more code wrapping it (hence the motivation for this package) or use the `<<-`
assignment operator (which sometimes binds a variable to a parent, and sometimes
to a global environment).

## Installation

You can install the development version of context from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("igor-sb/context")
```
