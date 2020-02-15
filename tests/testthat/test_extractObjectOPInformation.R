context("extractObjectOPInformation")

source_file <- 'sample-classes.R'
source_package <- 'wyz.code.offensiveProgramming'

f <- findFilesInPackage(source_file, source_package)
stopifnot(length(f) == 1)
source(f[1])

test_that("extractObjectOPInformation", {
  expect_error(extractObjectOPInformation(EmptyR6$new(), 'kkkfun'))
})
