library("data.table")
library("wyz.code.offensiveProgramming")
library("wyz.code.rdoc")

gc <- GenerationContext('inst/man-generated', overwrite_b_1 = TRUE,
                        verbosity_b_1 = FALSE, useMarkers_b_1 = FALSE)

target_package_name <- 'wyz.code.rdoc'

beautifier <- beautify()

pc <- ProcessingContext(
  extraneous_l = list(
    description = 'A fake description',
    keyword = 'documentation',
    concept = 'documentation generation'
  )
)

# 1. append
ic <- InputContext(NULL, 'append', packageName_s_1 = 'wyz.code.rdoc')
r1 <- produceManualPage(ic, pc, gc)

# 2. sum
ic <- InputContext(NULL, 'sum', packageName_s_1 = 'wyz.code.rdoc')
r2 <- produceManualPage(ic, pc, gc)

# 3. opfun
opfun <- function(x_l, x_s, x_b_1) { NA }
ic <- InputContext(NULL, 'opfun', packageName_s_1 = 'wyz.code.rdoc')
r3 <- produceManualPage(ic, pc, gc)

