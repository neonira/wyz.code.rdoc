## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)
source('vignette-common.R')

## ----context, eval = TRUE, echo = FALSE---------------------------------------
library("data.table")
library("wyz.code.offensiveProgramming")
library("wyz.code.rdoc", warn.conflicts = FALSE)

## ----function_mpg, eval = TRUE, echo = TRUE-----------------------------------
b <- beautify()
examples <- list(
  function() { append(1:5, 0:1, after = 3) }
)

ic <- InputContext(NULL, 'append')
pc <- ProcessingContext(
  extraneous_l = list(
    title = 'Vector Merging',
    description = sentensize('add elements to a vector'),
    details = paste('If the parameter', b$code('after'), 'is higher than' , 
                    b$code('x'), 'length,\n then insertion is done at the',
                    'end of the data structure'),
    references = paste('Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)', 
                 'The New S Language. Wadsworth & Brooks/Cole.'),
    examples = convertExamples(examples, captureOutput_b_1n = FALSE)
  ),
  postProcessing_l = list(
    arguments = function(content_s) {
      s <- sub('XXX_001', sentensize('the vector the values are to be appended to'), 
               content_s, fixed = TRUE)
      s <- sub('XXX_002', sentensize('to be included in the modified vector'), s, fixed = TRUE)
      s <- sub('XXX_003', 
               'a subscript, after which the values are to be appended',
               s, fixed = TRUE)
      s
    }
  )
)
td <- tempdir()
gc <- GenerationContext(td, overwrite = TRUE)
rv <- produceManualPage(ic, pc, gc)

## ----function_mpg_rv, eval = TRUE, echo = TRUE--------------------------------
readLines(rv$context$filename)

## ----op, eval = TRUE, echo = TRUE---------------------------------------------
iterateOverSet <- function(set_s, enforceUniqueness_b_1 = TRUE) {
  NULL
}

## ----op_mpg, eval = TRUE, echo = TRUE-----------------------------------------
examples <- list(
  function() { iterateOverSet(sample(LETTERS, 35, TRUE), FALSE) },
  function() { iterateOverSet(as.character(1:9)) }
)
ic <- InputContext(NULL, 'iterateOverSet')
pc <- ProcessingContext(
  extraneous_l = list(
    examples = convertExamples(examples, captureOutput_b_1n = FALSE)
  )
)
rv_op <- produceManualPage(ic, pc, gc)

## ----op_mpg_rv, eval = TRUE, echo = TRUE--------------------------------------
readLines(rv_op$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
source(findFilesInPackage('classes', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(MeltingPot_Env())
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(Zorg())
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(Bu_S3())
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
source(findFilesInPackage('Addition_TCFI_Partial_S3.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(Addition_TCFI_Partial_S3())
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(new('Person_S4', name = 'neonira'))
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
source(findFilesInPackage('Addition_TCFI_Partial_S4.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(new('Addition_TCFI_Partial_S4'))
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(new('Person_RC', name = 'neonira'))
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
source(findFilesInPackage('Addition_TCFI_Partial_RC.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(new('Addition_TCFI_Partial_RC'))
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(Accumulator_R6$new())
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
source(findFilesInPackage('Addition_TCFI_Partial_R6.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(Addition_TCFI_Partial_R6$new())
rv <- produceManualPage(ic, generationContext = gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(NULL, package = 'wyz.code.rdoc') # using an 
pc <- ProcessingContext(
  postProcessing_l = list(
    details = function(content_s) NULL
  )
)
rv <- produceManualPage(ic, pc, gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ic <- InputContext(dummy, dataFilename = 'dummy.csv')
pc <- ProcessingContext(
  extraneous_l = list(
    description = 'a dummy datafile for demonstration purpose',
    format = 'a data.frame 9x2',
    source = 'fake data - used only for demo'
  ),
  postProcessing_l = list(
    classification = function(content_s) NULL
  )
)
rv <- produceManualPage(ic, pc, gc)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
readLines(rv$context$filename)

