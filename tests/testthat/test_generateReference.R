context("generateReference")

o <- generateReference(
  list(url = 'https://neonira.github.io/offensiveProgrammingBook/',
       label = 'Offensive Programming Book')
)

test_that("generateReference", {
  expect_length(o, 1L)
  expect_equal(stringr::str_count(o, '\\{'), 2L)
})
