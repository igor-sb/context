#' Context manager
#'
#' @param on_enter function to run before entering context
#' @param on_exit function to run after taking actions
#' @param as Optional character vector specifying variables storing the return
#'  values of the `on_enter` function.
#' @param args Optional list of arguments supplied to `on_enter` function.
#' @return S3 class ContextManager
#' @export
create_context_manager <- function(
    on_enter = function() {},
    on_exit = function() {},
    args = NULL
) {
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
#' @param ... ContextAs S3 objects, followed by expression.
#'
#' @export
with <- function(...) {
  eval_environment <- parent.frame()
  args <- capture_arguments(...)
  contexts_as <- without_last_element(args) |> eval_list(eval_environment)
  expr <- get_last_element(args)
  tryCatch({
    contexts <- get_from_as(contexts_as, "context")
    as_variables <- get_from_as(contexts_as, "variable")
    on_enter_results <- get_enter_results(contexts)
    assign_in_environment(as_variables, on_enter_results, eval_environment)
    eval(expr, eval_environment)
  },
  finally = {
    run_on_exit_functions(contexts, on_enter_results)
    remove_from_environment(as_variables, eval_environment)
  })
  invisible()
}

#' Context manager's `as` function
#'
#' @param context Context manager constructor.
#' @param variable Variable name to use within the context.
#'
#' @return ContextAs S3 object  with elements `context` and `variable`.
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

remove_from_environment <- function(variable_names, environment) {
  for (variable_name in variable_names) {
    if (exists(variable_name, envir = environment)) {
      do.call("rm", list(x = variable_name, envir = environment))
    }
  }
}

assign_in_environment <- function(
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

get_from_as <- function(context_as_list, what) {
  lapply(
    context_as_list,
    function(context_as) context_as[[what]]
  )
}

get_enter_results <- function(contexts) {
  lapply(contexts, function(context) do.call(context$on_enter, context$args))
}
