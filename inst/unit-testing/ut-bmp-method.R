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
ic <- InputContext(pc, 'verifyPostProcessing', packageName_s_1 = 'wyz.code.rdoc')
r1 <- produceManualPage(ic, pc, gc)

# 2. ManualPageBuilder
# ic <- InputContext(m, 'interpretResults', packageName_s_1 = 'wyz.code.rdoc')
# r2 <- produceManualPage(ic, pc, gc)


WeirdNames <- function() {

  self <- environment()
  class(self) <- append('WeirdNames', class(self))

  `%*%` <- function(x_, y_) { Inf } # weird function name

  f <- function(x_d) x_d
  self
}

# 3. Weirdnames
r3 <- produceAllManualPagesFromObject(WeirdNames(), pc, gc, packageName_s_1 = 'wyz.code.rdoc')

