context("extractS4ObjectInformation")

source_files <- c(
  'sample-classes.R',
  'Addition_TCFI_Partial_S4.R'
)

source_package <- 'wyz.code.offensiveProgramming'

sapply(source_files, function(e) {
  f <- findFilesInPackage(e, source_package)
  stopifnot(length(f) == 1)
  source(f)
})

objects <- list(
  new('Person_S4'),
  new('FieldS4'),
  new('MethodS4'),
  new('Addition_TCFI_Partial_S4')
)

rv <- lapply(objects, extractS4ObjectInformation)

test_that("extractS4ObjectInformation", {
  myf <- function(k) {
    expect_length(rv[[!!k]], ifelse(k == 2L, 1L, 2L))
    expect_true(is.list(rv[[!!k]]))
  }

  lapply(seq_len(length(rv)), myf)
})
