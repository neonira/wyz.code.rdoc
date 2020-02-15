library("data.table")
library("wyz.code.offensiveProgramming")
library("wyz.code.rdoc")

gc <- GenerationContext('inst/man-generated', overwrite_b_1 = TRUE,
                        verbosity_b_1 = FALSE, useMarkers_b_1 = FALSE)

target_package_name <- 'wyz.code.rdoc'

beautifier <- beautify()

pc <- ProcessingContext(
  extraneous_l = list(
    seealso = sentensize(paste('class', beautifier$codelink('ProcessingContext'),
                               'and class',  beautifier$codelink('InputContext'))),
    keyword = 'documentation',
    concept = 'documentation generation'
  )
)

# 1. ProcessingContext
ic <- InputContext(pc, packageName_s_1 = 'wyz.code.rdoc')
r1 <- produceManualPage(ic, pc, gc)

# 2. ManualPageBuilder
m <- ManualPageBuilder(InputContext(NULL))
pc <- ProcessingContext(
  extraneous_l = list(
    seealso = sentensize(paste('class', beautifier$codelink('InputContext'),
                               'class', beautifier$codelink('ProcessingContext'),
                               'and class',  beautifier$codelink('GenerationContext'))),
    keyword = 'documentation',
    concept = 'documentation generation'
  )
)

ic <- InputContext(m, packageName_s_1 = 'wyz.code.rdoc')
r2 <- produceManualPage(ic, pc, gc)
interpretResults(r2)

