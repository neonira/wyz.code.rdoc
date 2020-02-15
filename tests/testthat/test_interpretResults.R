context("interpretResults")

test_that("interpretResults", {
  expect_error(interpretResults(NULL))
  expect_error(interpretResults(list()))
  expect_error(interpretResults(list(alpha = 3L, beta = 5.6)))
  expect_error(interpretResults(list(logic = 3L, context = 5.6)))
})
