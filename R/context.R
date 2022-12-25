#' Context manager
#'
#' @param on_enter function to run before entering context
#' @param on_exit function to run after taking actions
#' @param args Optional list of arguments supplied to `on_enter` function.
#' @return S3 class ContextManager
#' @export
create_context_manager <- function(
    on_enter,
    on_exit,
    args = NULL
) {
  stopifnot(
    "on_enter is not a function" = is.function(on_enter),
    "on_exit is not a function" = is.function(on_exit)
  )
  structure(
    list(
      on_enter = on_enter,
      on_exit = on_exit,
      args = args
    ),
    class = "ContextManager"
  )
}

#' Context manager's `with` function
#'
#' @param ... ContextAs S3 objects, followed by an expression.
#'
#' @export
with <- function(...) {
  args_list <- capture_arguments(...)
  check_with_args(...)
  eval_environment <- parent.frame()
  contexts_as <- without_last_element(args_list) |> eval_list(eval_environment)
  expr <- get_last_element(args_list)
  tryCatch({
    contexts <- get_from_as(contexts_as, "context")
    as_variables <- get_from_as(contexts_as, "variable")
    on_enter_returns <- run_on_enter_functions(contexts)
    assign_all_in_environment(as_variables, on_enter_returns, eval_environment)
    eval(expr, eval_environment)
  },
  finally = {
    run_on_exit_functions(contexts, on_enter_returns)
    remove_all_from_environment(as_variables, eval_environment)
  })
  invisible()
}

#' Context manager's `as` function
#'
#' @param context ContextManager S3 class or a character string.
#' @param variables Variable name to use within the context or a variable name
#'  in which to store the `context` if it is not a ContextManager S3 class.
#'
#' @return ContextAs S3 object  with elements `context` and `variable`.
#' @export
`%as%` <- function(context, variables) {
  UseMethod("%as%")
}

#' @export
`%as%.ContextManager` <- function(context, variables) {
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

#' @export
`%as%.default` <- function(context, variables) {
  variable_name <- rlang::enexpr(variables) |> as.character()
  assign(variable_name, context, envir = parent.frame())
  !is_end_of_file(context) && !is.null(context)
}

check_with_args <- function(...) {
  stopifnot("first argument is not ContextAs S3 object" =
              inherits(..1, "ContextAs") || inherits(..1, "ContextManager"))
}

capture_arguments <- function(...) {
  rlang::enexprs(...)
}

eval_list <- function(list, envir) {
  lapply(list, eval, envir = envir)
}

get_last_element <- function(list) {
  list[[length(list)]]
}

without_last_element <- function(list) {
  list[-length(list)]
}

run_on_exit_functions <- function(contexts, on_enter_results) {
  multi_apply(
    function(context, on_enter_result) {
      context$on_exit(on_enter_result)
    },
    contexts,
    on_enter_results
  )
}

multi_apply <- function(fun, ...) {
  invisible(mapply(
    fun,
    ...,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

remove_all_from_environment <- function(variable_names, environment) {
  for (variable_name in variable_names) {
    if (exists(variable_name, envir = environment)) {
      remove_from_environment(variable_name, environment)
    }
  }
}

remove_from_environment <- function(variable_name, environment) {
  do.call("rm", list(x = variable_name, envir = environment))
}

assign_all_in_environment <- function(
    variable_names,
    variable_values,
    environment
) {
  multi_apply(
    function(name, value, environment) {
      if (!is.null(value)) {
        assign(name, value, envir = environment)
      }
    },
    variable_names,
    variable_values,
    MoreArgs = list(environment = environment)
  )
}

get_from_as <- function(...) {
  UseMethod("get_from_as")
}

get_from_as.list <- function(context_as_list, what) {
  lapply(
    context_as_list,
    function(context_as) context_as[[what]]
  )
}

run_on_enter_functions <- function(contexts) {
  lapply(
    contexts,
    function(context) do.call(context$on_enter, context$args)
  )
}
