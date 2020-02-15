context("generateEnumeration")

a <- generateEnumeration(paste('case', 1:4))
b <- generateEnumeration(paste('case', 1:4), itemize_b_1 = TRUE)

test_that("generateEnumeration", {
  expect_true(is.character(a))
  expect_true(grepl('\\\\enumerate', a, perl = TRUE))
  expect_equal(stringr::str_count(a, '\\\\item '), 4L)

  expect_true(is.character(b))
  expect_true(grepl('\\\\itemize', b, perl = TRUE))
  expect_equal(stringr::str_count(b, '\\\\item '), 4L)

  expect_error(generateEnumeration(character(0)))
})
