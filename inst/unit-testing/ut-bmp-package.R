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

# 1. ProcessingContext
ic <- InputContext(NULL, packageName_s_1 = 'wyz.code.rdoc')
r1 <- produceManualPage(ic, pc, gc)


