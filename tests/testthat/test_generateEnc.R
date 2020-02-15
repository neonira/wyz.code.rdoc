context("generateEnc")

a <- generateEnc(list(list(text = 'Français', ascii = 'Francais')))

test_that("generateEnc", {
  expect_true(is.list(a))
  expect_length(a, 1)
  expect_equal(a[[1]], "\\enc{Français}{Francais}")
})
