test_that("open and read file line by line:", {
  temp_file <- tempfile()
  writeLines(LETTERS[1:5], temp_file)
  with(open_file(temp_file, "r", "f"), {
    for (i in 1:5) {
      line <- readLines(f, n = 1)
      expect_identical(line, LETTERS[i])
    }
  })
  invisible(file.remove(temp_file))
})

test_that("open file and catch an error:", {
  open_file_catch <- function(file_name, mode, as) {
    create_context_manager(
      on_enter = function(file_name, mode) file(file_name, open = mode),
      on_exit = function(file_connection) close(file_connection),
      on_error = function(error) stop("Cought an error"),
      as = as,
      args = list(file_name = file_name, mode = mode)
    )
  }
  temp_file <- tempfile()
  writeLines(LETTERS[1:5], temp_file)
  expect_error(
    with(open_file_catch(temp_file, "r", "f"), {
      stop()
    }),
    "Cought an error"
  )
  invisible(file.remove(temp_file))
})
