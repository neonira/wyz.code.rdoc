context("verifyDocumentationFile")

f <- findFilesInPackage('error.Rd', 'wyz.code.rdoc')
o <- verifyDocumentationFile(f[1])

g <- findFilesInPackage('warning.Rd', 'wyz.code.rdoc')
p <- verifyDocumentationFile(g[1])

test_that("verifyDocumentationFile", {
  expect_true(grepl('Error', o, fixed = TRUE))
  expect_true(grepl('checkRd', p, fixed = TRUE))
})
