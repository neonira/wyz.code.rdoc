context("extractClassificationInformation")

pkg <- 'wyz.code.offensiveProgramming'

test_that("extractClassificationInformation", {
  expect_null(extractClassificationInformation('non_existing_function', 'stats'))
  expect_null(extractClassificationInformation('non_existing_function', pkg))
  expect_error(extractClassificationInformation('generateStatusSummary', pkg)) # internal function
  expect_true(is.character(extractClassificationInformation('verifyFunctionName', pkg)))
})
