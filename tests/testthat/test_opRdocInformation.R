context("opRdocInformation")

test_that("opRdocInformation", {
  expect_true(is.data.table(opRdocInformation()))
})
