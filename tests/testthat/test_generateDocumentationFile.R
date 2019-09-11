context("generateDocumentationFile")

test_that("generateDocumentationFile - coverage", {
  expect_output(generateDocumentationFile(file.path(tempdir(), 'ex1'), 'content', TRUE, TRUE))
})
