context("identifyReplacementVariables")

f <- findFilesInPackage('generateEnumeration.Rd', 'wyz.code.rdoc')
o <- identifyReplacementVariables(f[1])

test_that("identifyReplacementVariables", {
  expect_true(is.list(o))
  # expect_null(o[[1]]) # issue covr
  expect_error(identifyReplacementVariables('/tmp/not-a-file.Rd'))
})
