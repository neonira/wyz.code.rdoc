context("produceDocumentationFile")

test_that("produceDocumentationFile - coverage", {
  expect_output(produceDocumentationFile(
    'ex1', 'content', GenerationContext(overwrite_b_1 = TRUE, verbosity_b_1 = TRUE)))
})
