context("manageSingleStrings")

f <- function() {}

test_that("manageSingleStrings", {
  expect_equal(manageSingleStrings(3L), '3L')
  expect_equal(manageSingleStrings('neonira'), '\"neonira\"')
  expect_equal(manageSingleStrings('alpha "beta"'), '\"alpha \\\"beta\\\"\"')
  expect_equal(manageSingleStrings(letters[1:3]), letters[1:3])
  expect_equal(manageSingleStrings(f), f)
  expect_equal(manageSingleStrings(call('+', 1, 1)), '1 + 1')

})
