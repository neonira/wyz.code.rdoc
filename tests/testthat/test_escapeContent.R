context("escapeContent")

p <- escapeContent('bla bla { }', escapeBraces_b_1 = TRUE)

test_that("escapeContent", {
  expect_equal(escapeContent('neonira@gmail.com') , 'neonira@@gmail.com')
  expect_equal(escapeContent('neonira@@gmail.com'), 'neonira@@gmail.com')
  expect_equal(escapeContent('neonira@@@gmail.com'), 'neonira@@@@gmail.com')
  expect_equal(escapeContent('neonira@@@@gmail.com'), 'neonira@@@@gmail.com')
  expect_equal(escapeContent('neonira@@@@@gmail.com'), 'neonira@@@@@@gmail.com')
  expect_equal(escapeContent('neonira@@@@@@gmail.com'), 'neonira@@@@@@gmail.com')

  expect_equal(escapeContent('x %% y == 3'), 'x \\%\\% y == 3')
  expect_equal(escapeContent('x \\%% y == 3'), 'x \\%\\% y == 3')
  expect_equal(escapeContent('x %\\% y == 3'), 'x \\%\\% y == 3')
  expect_equal(escapeContent('x \\%\\% y == 3'), 'x \\%\\% y == 3')

  expect_equal(escapeContent('x %\\% y == 3', TRUE), 'x \\%\\% y == 3') # coverage
  expect_equal(escapeContent('x \\%\\% y == 3', TRUE), 'x \\%\\% y == 3') # coverage

  expect_equal(escapeContent('function() { Inf }', TRUE), 'function() \\{ Inf \\}')
  expect_equal(escapeContent('function() \\{ Inf }', TRUE), 'function() \\{ Inf \\}')
  expect_equal(escapeContent('function() { Inf \\}', TRUE), 'function() \\{ Inf \\}')

  expect_equal(p, 'bla bla \\{ \\}')
})
