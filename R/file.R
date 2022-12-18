#' Open file implementation using a context manager
#'
#' Similar to Python's `open()`, intended to be used with a context manager.
#'
#' @param file_name File name to open.
#' @param mode File mode.
#'
#' @return S3 class ContextManager
#' @export
open <- function(file_name, mode = "r") {
  create_context_manager(
    on_enter = function(file_name, mode) file(file_name, open = mode),
    on_exit = function(file_connection) close(file_connection),
    args = list(file_name = file_name, mode = mode)
  )
}

