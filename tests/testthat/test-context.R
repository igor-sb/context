test_that("with-as single file:", {
  withr::with_tempfile("temp_file", {
    true_values <- LETTERS[1:5]
    test_values <- vector("character", 5)
    writeLines(true_values, temp_file)
    with(open(temp_file) %as% f, {
      for (i in 1:5) test_values[i] <- readLines(f, n = 1)
    })
    expect_identical(test_values, true_values)
  })
})

test_that("with-as multiple files:", {
  withr::with_tempfile(c("file1", "file2"), {
    true_values <- list(LETTERS[1:5], LETTERS[1:5 + 5])
    writeLines(true_values[[1]], file1)
    writeLines(true_values[[2]], file2)
    test_values <- list(vector("character", 5), vector("character", 5))
    with(open(file1) %as% f1, open(file2) %as% f2, {
      for (i in 1:5) {
        test_values[[1]][i] <- readLines(f1, n = 1)
        test_values[[2]][i] <- readLines(f2, n = 1)
      }
    })
    expect_identical(test_values, true_values)
  })
})

test_that("create_context_manager checks:", {
  expect_error(create_context_manager(1, function() return()),
               "on_enter is not a function")
  expect_error(create_context_manager(function() return(), "a"),
               "on_exit is not a function")
})

test_that("with-as invalid syntax:", {
  expect_error(with("2", 1),
               "first argument is not ContextAs S3 object")
})
