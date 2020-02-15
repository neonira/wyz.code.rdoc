context("generateMarkup")

x <- generateMarkup(c(letters[1:6]))
y <- generateMarkup(x, 'note')
z <- generateMarkup(c('x %% 2 == 0'))

a <- generateMarkup('a_b_c', 'item', c("bla bla bla x %% 2 == 0"))
b <- generateMarkup('a_b_c', 'item', c("bla bla bla x %% 2 == 0"), useSpace_b_1 = TRUE)

test_that("generateMarkup", {
  expect_equal(x, 'abcdef')
  expect_equal(y, '\\note{abcdef}')
  expect_equal(z, 'x \\%\\% 2 == 0')
  expect_equal(a, '\\item{a_b_c}{bla bla bla x \\%\\% 2 == 0}')
  expect_equal(b, '\\item{a_b_c} {bla bla bla x \\%\\% 2 == 0}')

  expect_error(generateMarkup('my content', 'zorg'))
  expect_error(generateMarkup('my content', 'zorg', 'another content'))
})

required_content <- list(
  double = c('href', 'enc', 'method', 'S3method', 'S4method',
             'item', 'tabular', 'section', 'subsection', 'if',
             'newcommand', 'renewcommand', 'deqn', 'eqn'),
  triple = c('ifelse')
)

test_that("generateMarkup - triple", {
  expect_equal(generateMarkup('my content', 'ifelse', 'content bis', content3_s = 'content ter'),
               '\\ifelse{my content}{content bis}{content ter}')

  expect_error(generateMarkup('my content', 'if', 'content bis', content3_s = 'content ter'))
  expect_error(generateMarkup('my content', 'ifelse', NA_character_, content3_s = 'content ter'))
  expect_error(generateMarkup('my content', 'ifelse', 'content bis')) # missing third content
})

test_that("generateMarkup - double", {
  lapply(required_content$double, function(e) {
    expect_true(grepl(e, generateMarkup('my content', e, 'content bis'), fixed = TRUE))
  })

  expect_error(generateMarkup('my content', 'ifelse', 'content ter'))
})

test_that("generateMarkup - mix", {
  expect_equal(generateMarkup('my content', 'href'), '\\href{my content}')
  expect_equal(generateMarkup('my content', 'href', 'content bis'), '\\href{my content}{content bis}')
})


mono <- setdiff(rdocKeywords(), unlist(required_content))
test_that("generateMarkup - single", {
  lapply(mono, function(e) {
    expect_true(grepl(e, generateMarkup('my content', e), fixed = TRUE))
    expect_error(generateMarkup('my content', e, 'bis'))
  })
})

