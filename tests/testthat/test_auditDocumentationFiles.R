context("auditDocumentationFiles")

f <- findFilesInPackage('generateEnumeration.Rd', 'wyz.code.rdoc')
o <- auditDocumentationFiles(dirname(f))

test_that("auditDocumentationFiles", {
  expect_length(o, 2L)
  expect_true(is.list(o))
  expect_equal(names(o), c('correct', 'incorrect'))
})
