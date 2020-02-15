context("generateParagraph2NL")
n <- 7
s <- generateParagraph2NL(LETTERS[1:n])
test_that("generateParagraph2NL", {
  expect_length(s, 1L)
  expect_equal(stringr::str_count(s, '\n'), 2L * (n - 1L))
})
