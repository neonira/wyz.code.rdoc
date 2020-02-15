context("generateS3MethodSignature")

o <- generateS3MethodSignature('opfun', 'zorg', c('x_n', 'z_f'))
test_that("generateS3MethodSignature", {
  expect_length(o, 1L)
  expect_equal(o, "\\method{opfun}{zorg}(x_n, z_f)")
})
