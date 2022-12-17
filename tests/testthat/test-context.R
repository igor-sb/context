test_that("open and read file line by line:", {
  set.seed(42)
  temp_file <- tempfile()
  writeLines(LETTERS[1:5], temp_file)
  with(open_file(temp_file, "r") %as% f, {
    for (i in 1:5) {
      line <- readLines(f, n = 1)
      expect_identical(line, LETTERS[i])
    }
  })
  invisible(file.remove(temp_file))
})

test_that("open and read file line by line, list context:", {
  set.seed(42)
  temp_file <- tempfile()
  writeLines(LETTERS[1:5], temp_file)
  with(list(open_file(temp_file, "r") %as% f), {
    for (i in 1:5) {
      line <- readLines(f, n = 1)
      expect_identical(line, LETTERS[i])
    }
  })
  invisible(file.remove(temp_file))
})

test_that("open file and catch an error:", {
  open_file_catch <- function(file_name, mode) {
    create_context_manager(
      on_enter = function(file_name, mode) file(file_name, open = mode),
      on_exit = function(file_connection) close(file_connection),
      on_error = function(error) stop("Caught an error"),
      args = list(file_name = file_name, mode = mode)
    )
  }
  temp_file <- tempfile()
  writeLines(LETTERS[1:5], temp_file)
  expect_error(
    with(open_file_catch(temp_file, "r") %as% f, {
      stop()
    }),
    "Caught an error"
  )
  invisible(file.remove(temp_file))
})

test_that("open and read file line by line (multiple)", {
  set.seed(42)
  file1 <- tempfile()
  file2 <- tempfile()
  writeLines(LETTERS[1:5], file1)
  writeLines(LETTERS[6:10], file2)
  with(list(open_file(file1, "r") %as% f1, open_file(file2, "r") %as% f2), {
    for (i in 1:5) {
      cat(readLines(f1, n = 1), " <-> ", readLines(f2, n = 1))
    }
  })
  invisible(file.remove(file1, file2))
})
