context("extractR6ObjectInformation")

source_files <- c(
  'sample-classes.R',
  'Addition_TCFI_Partial_R6.R'
)

source_package <- 'wyz.code.offensiveProgramming'

sapply(source_files, function(e) {
  f <- findFilesInPackage(e, source_package)
  stopifnot(length(f) == 1)
  source(f)
})

objects <- list(
  EmptyR6$new(),
  FieldR6$new(),
  MethodR6$new(),
  BindingR6$new(),
  Accumulator_R6$new(),
  Addition_TCFI_Partial_R6$new()
)

rv <- lapply(objects, extractR6ObjectInformation)

test_that("extractR6ObjectInformation", {
  myf <- function(k) {
    expect_length(rv[[!!k]], c(1, 2, 2, 2, 3, 3)[k])
    expect_true(is.list(rv[[!!k]]))
  }

  lapply(seq_len(length(rv)), myf)
})
