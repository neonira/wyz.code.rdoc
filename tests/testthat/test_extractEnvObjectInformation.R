context("extractEnvObjectInformation")

source_files <- c(
  'sample-classes.R',
  'AdditionTCFIP.R'
)

source_package <- 'wyz.code.offensiveProgramming'

sapply(source_files, function(e) {
  f <- findFilesInPackage(e, source_package)
  stopifnot(length(f) == 1)
  source(f)
})

objects <- list(
  MyEnv(),
  FieldEnv(),
  MethodEnv(),
  EmptyEnv(),
  Zarg(),
  Zirg(),
  Zorg(),
  Zurg(),
  AdditionTCFIP()
)

rv <- lapply(objects, extractEnvObjectInformation)

test_that("extractEnvObjectInformation", {
  myf <- function(k) {
    expect_length(rv[[!!k]], ifelse(k %in% c(2, 4), 1L, 2L))
    expect_true(is.list(rv[[!!k]]))
  }

  lapply(seq_len(length(rv)), myf)
})
