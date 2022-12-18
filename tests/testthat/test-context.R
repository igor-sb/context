test_that("open and read file line by line:", {
  set.seed(42)
  temp_file <- tempfile()
  writeLines(LETTERS[1:5], temp_file)
  with(open(temp_file) %as% f, {
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
  with(
    list(open(temp_file) %as% f), {
      for (i in 1:5) {
        line <- readLines(f, n = 1)
        expect_identical(line, LETTERS[i])
      }
    }
  )
  invisible(file.remove(temp_file))
})

test_that("open and read file line by line (multiple)", {
  set.seed(42)
  files <- c(tempfile(), tempfile())
  true_values <- list(LETTERS[1:5], LETTERS[1:5 + 5])
  writeLines(true_values[[1]], files[1])
  writeLines(true_values[[2]], files[2])
  test_values <- list(vector("character", 5), vector("character", 5))
  with(
    list(
      open(files[1]) %as% f1,
      open(files[2]) %as% f2
    ), {
      for (i in 1:5) {
        test_values[[1]][i] <- readLines(f1, n = 1)
        test_values[[2]][i] <- readLines(f2, n = 1)
      }
  })
  expect_identical(true_values, test_values)
  invisible(file.remove(unlist(files)))
})

test_that("open and read file line by line (multiple) with2", {
  set.seed(42)
  files <- c(tempfile(), tempfile())
  true_values <- list(LETTERS[1:5], LETTERS[1:5 + 5])
  writeLines(true_values[[1]], files[1])
  writeLines(true_values[[2]], files[2])
  test_values <- list(vector("character", 5), vector("character", 5))
  with2(open(files[1]) %as% f1, open(files[2]) %as% f2, {
    for (i in 1:5) {
      test_values[[1]][i] <- readLines(f1, n = 1)
      test_values[[2]][i] <- readLines(f2, n = 1)
    }
  })
  expect_identical(true_values, test_values)
  invisible(file.remove(unlist(files)))
})
