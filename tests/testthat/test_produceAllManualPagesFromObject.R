context("produceAllManualPagesFromObject")

gc <- GenerationContext(tempdir(), overwrite_b_1 = TRUE,
                        verbosity_b_1 = FALSE, useMarkers_b_1 = FALSE)

target_package_name <- 'wyz.code.rdoc'

pc <- ProcessingContext(
  extraneous_l = list(
    keyword = 'documentation',
    concept = 'documentation generation'
  )
)

options('rdhoc_hack' = TRUE)

# 1. ProcessingContext
r1 <- produceAllManualPagesFromObject(pc, pc, gc, target_package_name) # not an error pc, pc, gc - desired

# 2. ManualPageBuilder
gcv <- GenerationContext(tempdir(), overwrite_b_1 = TRUE,
                         verbosity_b_1 = TRUE, useMarkers_b_1 = TRUE)
m <- ManualPageBuilder(InputContext(NULL))
r2 <- produceAllManualPagesFromObject(m, pc, gcv, target_package_name)

test_that("produceAllManualPagesFromObject", {
  expect_true(is.list(r1))
  expect_true(is.list(r2))
  expect_error(
    produceAllManualPagesFromObject(NULL, packageName_s_1 = target_package_name)
  )
})
