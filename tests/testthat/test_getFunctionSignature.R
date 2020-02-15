context("getFunctionSignature")

# following work nice interactively but fails in testing context.
# g <- function(d, a = pi, b = append, f = function(x_, y_) {}, ...) {}
# f <- function(w = list(a = pi, b = append), z = letters[1:3], f = function(x_, y_) {}, ...) {}
# print(search())
# fs <- getFunctionSignature('f')
# gs <- getFunctionSignature('g')


test_that("getFunctionSignature", {
  expect_error(getFunctionSignature('notAFunction'))
  expect_true(is.character(getFunctionSignature('append')))
})


