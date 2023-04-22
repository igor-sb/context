#' Open file implementation using a context manager
#'
#' Similar to Python's `open()`, but creates a ContextManager S3 object.
#' Intended to be used as a first (left) argument of `%as%`, or as any but last
#' argument of `context::with()`.
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

#' Read a single line from file connectoin
#'
#' @param connection File connection.
#'
#' @return Character string of one line in a file.
#' @export
read_line <- function(connection) {
  readLines(connection, n = 1)
}

is_end_of_file <- function(line) {
  length(line) == 0
}
