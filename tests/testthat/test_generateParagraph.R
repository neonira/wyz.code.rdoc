context("generateParagraph")
n <- 7
s <- generateParagraph(LETTERS[1:n])
p <- generateParagraph(LETTERS[1:n], addFinalSeparator_b_1 = TRUE)
test_that("generateParagraph", {
  expect_length(s, 1L)
  expect_equal(stringr::str_count(s, '\n'), n - 1L)
  expect_equal(stringr::str_count(p, '\n'), n)
})
