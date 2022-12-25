test_that("as.default", {
  test_return <- "test_value" %as% test_variable
  expect_true(test_return)
  expect_identical(test_variable, "test_value")
})

test_that("read_line", {
  expect_identical(
    read_line(read_test_fastq()),
    "@test.fastq.1008 HWI-EAS100R:1:1:1883:428 length=15"
  )
})

test_that("is_end_of_life", {
  withr::with_tempfile("temp_file", {
    true_values <- LETTERS[1:5]
    test_values <- vector("logical", 6)
    writeLines(true_values, temp_file)
    with(open(temp_file) %as% f, {
      for (i in 1:6) {
        line <- readLines(f, n = 1)
        test_values[i] <- is_end_of_file(line)
      }
    })
    expect_identical(test_values, c(rep(FALSE, 5), TRUE))
  })
})

test_that("check test.fastq", {
  test_fastq <- readLines(read_test_fastq())
  true_fastq <- c(
    "@test.fastq.1008 HWI-EAS100R:1:1:1883:428 length=15",
    "AGAATACTTCCGGCC",
    "+",
    "zbte7#0|\"L14Mt_",
    "@test.fastq.2053 HWI-EAS100R:1:1:1565:873 length=15",
    "CGTNGANCGGCGATG",
    "+",
    "Ny.!Pt!D#`7x\\A'",
    "@test.fastq.3014 HWI-EAS100R:1:1:424:1529 length=15",
    "GACATTCTGCTGTNC",
    "+",
    "jBLfvOLn{AHrS!,",
    "@test.fastq.3823 HWI-EAS100R:1:1:1567:1928 length=15",
    "GCTATCCTCGACGGC",
    "+",
    "Fb0T;%QJqtoYh@e",
    "@test.fastq.4637 HWI-EAS100R:1:1:183:940 length=15",
    "NGAAGGAGGTCTCCT",
    "+",
    "!@Az5,uB!$~x`vi"
  )
  expect_identical(test_fastq, true_fastq)
})
