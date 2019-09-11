context("generateSection")

a <- generateSection('title', 'a title')
b <- generateSection('warning', c('some bla bla bla for warning',
                                  'some other bla bla bla for warning'))

test_that("generateSection", {
  expect_equal(a, '\\title{a title}')
  expect_equal(b, '\\section{warning}{\nsome bla bla bla for warning\nsome other bla bla bla for warning\n}')
})
