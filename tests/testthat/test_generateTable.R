context("generateTable")

dt <- data.table::data.table(x = runif(3), y = letters[1:3])

a <- generateTable(dt)
b <- generateTable(dt, numberRows_b_1 = TRUE)

test_that("generateTable", {
  expect_error(generateTable(dt, 'alvq')) # bad length
  expect_error(generateTable(dt, 'sr')) # bad alignement specification
  expect_error(generateTable(dt[0])) # no data
  expect_error(generateTable(data.table())) # no data

  expect_true(is.character(a))
  expect_equal(stringr::str_count(a, '\n'), 5L)
  expect_equal(stringr::str_count(a, '\\\\tab '), 4L)

  expect_true(is.character(b))
  expect_equal(stringr::str_count(b, '\n'), 5L)
  expect_equal(stringr::str_count(b, '\\\\tab '), 7L)
})
