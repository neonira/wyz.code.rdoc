context("InputContext")

options('rdhoc_hack' = TRUE)

ic <- InputContext(dummy, dataFilename_s_1 = 'dummy.R')
ic$setUseMarkers(TRUE)

test_that("InputContext - data", {
  expect_true(nchar(ic$produceFormat()) > 3L)
  expect_true(!is.null(ic$produceSource()))
  expect_true(is.character(ic$getName()))
})

ic$setUseMarkers(FALSE)

test_that("InputContext - data", {
  expect_true(nchar(ic$produceFormat()) > 3L)
  expect_true(is.null(ic$produceSource()))
})

ic1 <- InputContext(ProcessingContext(), methodName_s_1 = 'verifyPostProcessing')
ic1$setUseMarkers(TRUE)
ic2 <- InputContext(ProcessingContext())
test_that("InputContext", {
  expect_true(nchar(ic1$produceDescription()) > 3L)
  expect_true(nchar(ic2$produceDescription()) > 3L)
  expect_true(is.null(ic2$produceSource()))
  expect_true(is.null(ic2$produceFormat()))
  expect_true(is.character(ic1$getName()))
  expect_true(is.character(ic2$getName()))
})

options('rdhoc_hack' = FALSE)

source_file <- 'Addition_TCFI_Partial_R6.R'
source_package <- 'wyz.code.offensiveProgramming'

f <- findFilesInPackage(source_file, source_package)
stopifnot(length(f) == 1)
source(f)

o <- Addition_TCFI_Partial_R6$new()
ic3 <- InputContext(o, methodName_s_1 = 'addInteger')
ic3$setUseMarkers(TRUE)

ic4 <- InputContext(o)
ic4$setUseMarkers(TRUE)

test_that("InputContext", {
  expect_true(nchar(ic3$produceDescription()) > 3L)
  expect_true(nchar(ic4$produceDescription()) > 3L)
})

source_file <- 'sample-classes.R'

f <- findFilesInPackage(source_file, source_package)
stopifnot(length(f) == 1)
source(f)

o <- EmptyEnv()
ic5 <- InputContext(o, methodName_s_1 = 'addInteger')
ic5$setUseMarkers(TRUE)

ic6 <- InputContext(Accumulator_R6$new(), methodName_s_1 = 'addInteger')
ic6$setUseMarkers(FALSE)

ic7 <- InputContext(NULL, 'sum', packageName_s_1 = 'slot')
ic7$setUseMarkers(TRUE)

test_that("InputContext", {
  expect_true(nchar(ic5$produceDescription()) > 3L)
  expect_true(is.null(ic6$produceDescription()))
  expect_true(is.character(InputContext(NULL, packageName_s_1 = 'slot')$getName()))
  expect_true(is.character(ic7$getName()))
  expect_true(is.character(ic7$produceDescription()))
})
