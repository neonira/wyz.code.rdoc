context("getObjectConstructor")

test_that("getObjectConstructor", {
  expect_true(is.na(getObjectConstructor(4L)))
})
