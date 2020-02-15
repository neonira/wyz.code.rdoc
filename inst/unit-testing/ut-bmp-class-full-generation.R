library("data.table")
library("wyz.code.offensiveProgramming")
library("wyz.code.rdoc")

gc <- GenerationContext('inst/man-generated', overwrite_b_1 = TRUE,
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
m <- ManualPageBuilder(InputContext(NULL))
r2 <- produceAllManualPagesFromObject(m, pc, gc, target_package_name)

