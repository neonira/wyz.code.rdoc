context("sentensize")

o <- sapply(c('', 'neonira wrote package \twyz.code.rdoc'), sentensize, USE.NAMES = FALSE)

test_that("sentensize", {
expect_length(o[1], 1L)
expect_equal(o[1], '')
expect_length(o[2], 1L)
expect_equal(o[2], 'Neonira wrote package wyz.code.rdoc.')
})
