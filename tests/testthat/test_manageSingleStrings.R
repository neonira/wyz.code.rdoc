context("manageSingleStrings")

f <- function() {}

test_that("manageSingleStrings", {
  expect_equal(manageSingleStrings(3L), '3L')
  expect_equal(manageSingleStrings('neonira'), '\"neonira\"')
  expect_equal(manageSingleStrings('alpha "beta"'), '\"alpha \\\"beta\\\"\"')
  expect_equal(manageSingleStrings(letters[1:3]), letters[1:3])
  expect_error(manageSingleStrings(f))
  expect_equal(manageSingleStrings(call('+', 1, 1)), '1 + 1')

  expect_equal(manageSingleStrings(NA_character_), 'NA_character_')
  expect_equal(manageSingleStrings(NA_integer_), 'NA_integer_')
  expect_equal(manageSingleStrings(NA_real_), 'NA_real_')
  expect_equal(manageSingleStrings(NA_complex_), 'NA_complex_')
  expect_equal(manageSingleStrings(NA), 'NA')

  expect_true(all.equal(manageSingleStrings(1:3), paste0(1:3, "L")))
})
