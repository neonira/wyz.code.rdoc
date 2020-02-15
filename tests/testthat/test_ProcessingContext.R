context("ProcessingContext")

test_that("ProcessingContext", {
  expect_error(ProcessingContext(1L))
  expect_error(ProcessingContext(list(), 2L))
})
