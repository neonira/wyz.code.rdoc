context("generateOptionLink")

l <- generateOptionLink('my.package', 'entry')

test_that("generateOptionLink", {
  expect_length(l, 1L)
  expect_equal(l, "\\link[my.package]{entry}")
})

p <- producePackageLink('my.package', 'entry')

test_that("generateOptionLink", {
  expect_length(p, 1L)
  expect_equal(p, "\\link[my.package:entry]{my.package:entry}")
})


s <- generateOptionSexpr('echo=TRUE', 'x <- 1')

test_that("generateOptionSexpr", {
  expect_length(s, 1L)
  expect_equal(s, "\\Sexpr[echo=TRUE]{x <- 1}")
})


