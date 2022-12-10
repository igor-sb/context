#' Context manager
#'
#' @param on_enter function to run before entering context
#' @param on_exit function to run after taking actions
#' @param on_error optional function to run if an error is thrown
#' @param as Optional character vector specifying variables storing the return
#'  values of the `on_enter` function.
#' @param args Optional list of arguments supplied to `on_enter` function.
#' @return S3 class ContextManager
#' @export
create_context_manager <- function(
    on_enter = function() {},
    on_exit = function() {},
    on_error = NULL,
    as = NULL,
    args = NULL
) {
  structure(
    list(
      on_enter = on_enter,
      on_exit = on_exit,
      on_error = on_error,
      as = as,
      args = args
    ),
    class = "ContextManager"
  )
}

#' Context manager's `with` function
#'
#' @param context [`ContextManager`] object
#' @param expression Expression or code block to be evaluated within the context
#'
#' @export
with <- function(context, expression) {
  parent_environment <- parent.frame()
  enter_result <- do.call(context$on_enter, context$args)
  on.exit({
    context$on_exit(enter_result)
    remove_variable_from_environment(context$as, parent_environment)
  })
  set_variable_in_environment(context$as, enter_result, parent_environment)
  evaluate_expression(
    substitute(expression),
    parent_environment,
    context$on_error
  )
}

set_variable_in_environment <- function(name, value, environment) {
  if (!is.null(value)) {
    assign(name, value, envir = environment)
  }
}

remove_variable_from_environment <- function(variable, environment) {
  if (exists(variable, envir = environment)) {
    do.call("rm", list(x = variable, envir = environment))
  }
}

evaluate_expression <- function(expression, environment, error_fun) {
  if (is.function(error_fun)) {
    tryCatch(eval(expression, environment), error = error_fun)
  } else {
    eval(expression, environment)
  }
}

#' Open file implementation using a context manager
#'
#' Similar to Python's `open()`, intended to be used with a context manager.
#'
#' @param file_name File name to open.
#' @param mode File mode.
#' @param as Character specifying the name of the variable.
#'
#' @return S3 class ContextManager
#' @export
open_file <- function(file_name, mode, as) {
  create_context_manager(
    on_enter = function(file_name, mode) file(file_name, open = mode),
    on_exit = function(file_connection) close(file_connection),
    on_error = NULL,
    as = as,
    args = list(file_name = file_name, mode = mode)
  )
}
