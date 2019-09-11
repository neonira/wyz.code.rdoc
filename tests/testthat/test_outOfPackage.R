context("outOfPackage")

test_that("outOfPackage - coverage", {
  expect_true('error' %in% class(guardExecution(abort('volontary aborption'), FALSE)))

  expect_error(ensureFilenameExtension('x', character(0)))
  expect_equal(ensureFilenameExtension('x', 'R'), 'x.R')
  expect_equal(ensureFilenameExtension('x', ''), 'x.')
  expect_equal(ensureFilenameExtension('a', NA_character_), 'a')
})
