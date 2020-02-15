context("getStandardSectionNames")

ss <- getStandardSectionNames()

test_that("getStandardSectionNames", {
  expect_length(ss, 22)
  expect_true('custom_section' %in% ss)
  expect_true(is.unsorted(ss))
  expect_true(!is.unsorted(getStandardSectionNames(TRUE)))
})
