
# context

<!-- badges: start -->
<!-- badges: end -->

Context manager package `context` provides a runtime context, like the one in
Python:

```r
library(context)

with(open(filename) %as% f, {
  do_stuff(f)
})
```

This creates a connection and binds it to the variable `f` in the current
environment. This is equivalent to the Python code:

```py
with open(filename) as f:
  do_stuff(f)
```

## How?

ContextManager S3 object is created using `create_context_manager()` function,
which takes 3 arguments:
* `on_enter`: function to evaluate on entering context. Return value of this
  function is stored in the variable on the right side of the `%as%` operator.
* `on_exit`: function to evaluate on exiting context.
* `args`: arguments passed on the `on_enter` function.

This can be use to create custom context managers, for example:

```r
open_custom <- function(file_name, mode = "r") {
  create_context_manager(
    on_enter = function(file_name, mode) open_file(file_name, open = mode),
    on_exit = function(file_connection) close(file_connection),
    args = list(file_name = file_name, mode = mode)
  )
}

```

## Installation

You can install the development version of context from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("igor-sb/context")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(context)
## basic example code
```

