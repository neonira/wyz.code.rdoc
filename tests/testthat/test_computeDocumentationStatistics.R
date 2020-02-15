context("computeDocumentationStatistics")

f <- findFilesInPackage('generateEnumeration.Rd', 'wyz.code.rdoc')
o <- computeDocumentationStatistics(f[1])

test_that("computeDocumentationStatistics", {
  expect_true(data.table::is.data.table(o))
  expect_equal(ncol(o), 3L)
})
