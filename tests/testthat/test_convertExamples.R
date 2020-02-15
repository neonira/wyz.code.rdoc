context("convertExamples")

someComputation <- function(numberAsString_s_1) {
  suppressWarnings(sum(as.integer(strsplit(numberAsString_s_1, '')[[1]]), na.rm = TRUE))
}

examples <- list(
  function() {
    someComputation("145")
  },
  function() {
    someComputation("1547215")
  },
  function() {
    someComputation(NA_character_)
  },
  function() {
    invisible(someComputation("0x145ABC"))
  }
)

p <- convertExamples(examples, TRUE , c(NA_character_, 'donttest', 'dontrun', 'dontshow'))
o <- convertExamples(examples, FALSE, c(NA_character_, 'donttest', 'dontrun', 'dontshow'))

test_that("convertExamples", {
  expect_length(p, 1)
  expect_true(stringr::str_count(p, '\n') == 18)

  expect_length(o, 1)
  expect_true(stringr::str_count(o, '\n') == 14)

  expect_error(convertExamples(NA, TRUE))
  expect_error(convertExamples(as.list(letters[1:3]), TRUE))
  expect_error(convertExamples(list(function(x) {x}), TRUE))
})
