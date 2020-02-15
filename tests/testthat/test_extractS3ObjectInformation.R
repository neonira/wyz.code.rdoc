context("extractS3ObjectInformation")

source_files <- c(
  'sample-classes.R',
  'Addition_TCFI_Partial_S3.R'
)

source_package <- 'wyz.code.offensiveProgramming'

sapply(source_files, function(e) {
  f <- findFilesInPackage(e, source_package)
  stopifnot(length(f) == 1)
  source(f)
})

objects <- list(
  Bu_S3(),
  FieldS3(),
  MethodS3(),
  Addition_TCFI_Partial_S3()
)

rv <- lapply(objects, extractS3ObjectInformation)

test_that("extractS3ObjectInformation", {
  myf <- function(k) {
    expect_length(rv[[!!k]], ifelse(k %in% 2:3, 1L, 2L))
    expect_true(is.list(rv[[!!k]]))
  }

  lapply(seq_len(length(rv)), myf)
})
