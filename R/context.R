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
    args = NULL
) {
  structure(
    list(
      on_enter = on_enter,
      on_exit = on_exit,
      on_error = on_error,
      args = args
    ),
    class = "ContextManager"
  )
}

#' Context manager's `with` function
#'
#' @param data [`ContextAs`] S3 object with elements `context` and `variable`
#' @param expr Expression or code block to be evaluated within the context
#' @param ... Unused. Here for consistency with `base::with()`.
#'
#' @export
with.ContextAs <- function(data, expr, ...) {
  keyword_args <- list(...)
  eval_environment <- if ("environment" %in% names(keyword_args)) {
    keyword_args$environment
  } else {
    parent.frame()
  }
  context <- data$context
  as_variable <- data$variable
  tryCatch({
    enter_result <- get_enter_result(context$on_enter, context$args)
    set_variable_in_environment(as_variable, enter_result, eval_environment)
    eval(expr, eval_environment)
  },
  error = context$on_error,
  finally = {
    context$on_exit(enter_result)
    remove_variable_from_environment(as_variable, eval_environment)
  })
}

# This won't work. This will evaluate expr in each list element separately
with.list <- function(data, expr, ...) {
  # Run tryCatch by vectorizing set_variable_in_environment
  #  get_enver_result
  #  remove_variable_from_environment
  #
  calling_environment <- parent.frame()
  invisible(lapply(
    data,
    with.ContextAs,
    expr = expr,
    environment = calling_environment
  ))
}

get_enter_result <- function(on_enter_fun, on_enter_args) {
  do.call(on_enter_fun, on_enter_args)
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

#' Context manager's `as` function
#'
#' @param context Context manager constructor.
#' @param variable Variable name to use within the context.
#'
#' @return List with elements `context` and `variable`.
#' @export
`%as%` <- function(context, variables) {
  variables_symbols <- rlang::ensyms(variables)
  variables_strings <- sapply(variables_symbols, rlang::as_string)
  structure(
    list(
      context = context,
      variable = variables_strings
    ),
    class = "ContextAs"
  )
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
open_file <- function(file_name, mode) {
  create_context_manager(
    on_enter = function(file_name, mode) file(file_name, open = mode),
    on_exit = function(file_connection) close(file_connection),
    on_error = NULL,
    args = list(file_name = file_name, mode = mode)
  )
}

# Multiple: Vectorize open_file and %as% ?
