context("generateParagraphCR")
n <- 7
s <- generateParagraphCR(LETTERS[1:n])
test_that("generateParagraphCR", {
  expect_length(s, 1L)
  expect_equal(stringr::str_count(s, '\\\\cr'), n - 1L)
})
