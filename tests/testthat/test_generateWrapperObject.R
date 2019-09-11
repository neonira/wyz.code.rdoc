context("generateWrapperObject")

test_that("generateWrapperObject", {
  expect_true(is.object(generateWrapperObject(sum)))
  expect_true(is.object(generateWrapperObject(sum, 'sum')))
})
