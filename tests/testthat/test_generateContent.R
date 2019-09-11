context("generateContent")

x <- generateContent(c(letters[1:6]))
y <- generateContent(x, 'note')
z <- generateContent(c('x %% 2 == 0'))

a <- generateContent('a_b_c', 'item', c("bla bla bla x %% 2 == 0"))
b <- generateContent('a_b_c', 'item', c("bla bla bla x %% 2 == 0"), useSpace_b_1 = TRUE)

test_that("generateContent", {
  expect_equal(x, 'abcdef')
  expect_equal(y, '\\note{abcdef}')
  expect_equal(z, 'x %%%% 2 == 0')
  expect_equal(a, '\\item{a_b_c}{bla bla bla x %%%% 2 == 0}')
  expect_equal(b, '\\item{a_b_c} {bla bla bla x %%%% 2 == 0}')

  expect_error(generateContent('my content', 'zorg'))
  expect_error(generateContent('my content', 'zorg', 'another content'))
})
