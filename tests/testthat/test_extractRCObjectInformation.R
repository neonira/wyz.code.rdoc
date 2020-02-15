context("extractRCObjectInformation")

source_files <- c(
  'sample-classes.R',
  'Addition_TCFI_Partial_RC.R'
)

source_package <- 'wyz.code.offensiveProgramming'

sapply(source_files, function(e) {
  f <- findFilesInPackage(e, source_package)
  stopifnot(length(f) == 1)
  source(f)
})

objects <- list(
  new('Person_RC', name = 'neonira'),
  new('Addition_TCFI_Partial_RC'),
  new('EmptyRC'),
  new('FieldRC'),
  new('MethodRC')
)

rv <- lapply(objects, extractRCObjectInformation)

test_that("extractRCObjectInformation", {
  lapply(seq_len(length(rv)), function(k) {
    expect_length(rv[[!!k]], ifelse(k == 3L, 1L, ifelse(k > 3L, 2L, 3L)))
    expect_true('RC class definition' %in% names(rv[[!!k]]))
    if (!k %in% c(3L, 5L))
      expect_true('RC fields' %in% names(rv[[!!k]]))

    if (!k %in% c(3L, 4L))
     expect_true('RC methods' %in% names(rv[[!!k]]))
  })
})

test_that("extractRCObjectInformation", {
  expect_length(extractRCObjectInformation(new('EmptyRC')), 1L)
})
