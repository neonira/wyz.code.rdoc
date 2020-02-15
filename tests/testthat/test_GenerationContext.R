context("GenerationContext")

test_that("GenerationContext", {
  expect_error(GenerationContext('/NotExistingFolder'))
})
