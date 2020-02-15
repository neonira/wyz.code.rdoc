context("produceManualPage")

gc <- GenerationContext(tempdir(), overwrite_b_1 = TRUE,
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
ic <- InputContext(NULL, 'append', packageName_s_1 = target_package_name)
r1 <- produceManualPage(ic, pc, gc)

# 2. sum
ic <- InputContext(NULL, 'sum', packageName_s_1 = target_package_name)
r2 <- produceManualPage(ic, pc, gc)

# 3. OP instrumented function - semantic naming
gcv <- GenerationContext(tempdir(), overwrite_b_1 = TRUE,
                         verbosity_b_1 = TRUE, useMarkers_b_1 = FALSE)
ic <- InputContext(NULL, 'findFilesInPackage', packageName_s_1 = target_package_name)
r3 <- produceManualPage(ic, pc, gcv)

# 4. package manual page
ic <- InputContext(NULL, packageName_s_1 = target_package_name)
r4 <- produceManualPage(ic, pc, gc)

# 5. file dummy.R
ic <- InputContext(dummy,
                   packageName_s_1 = target_package_name,
                   dataFilename_s_1 = 'dummy.R')

r5 <- produceManualPage(ic, pc, gc)



source_files <- c(
  'sample-classes.R',
  'Addition.R',
  'AdditionFIPartial.R',
  'AdditionFI.R',
  'AdditionTCPartial.R',
  'AdditionTC.R',
  'AdditionTCFIP.R',
  'Addition_TCFI_Partial_R6.R',
  'Addition_TCFI_Partial_S3.R',
  'Addition_TCFI_Partial_RC.R',
  'Addition_TCFI_Partial_S4.R',
  'AdditionTCFIG1.R'
)

source_package <- 'wyz.code.offensiveProgramming'

sapply(source_files, function(e) {
  f <- findFilesInPackage(e, source_package)
  stopifnot(length(f) == 1)
  source(f)
})

objects <- list( MyEnv(),
                 Bu_S3(),
                 new('Person_RC', name = 'neonira'),
                 new('Person_S4', name = 'neonira'),
                 Wyx(1:7),
                 Accumulator_R6$new(),
                 Zarg(),
                 Zirg(),
                 Zorg(),
                 Zurg(),
                 PureR(),
                 Addition(),
                 AdditionFI(),
                 AdditionTC(),
                 AdditionTCFIG1(),
                 AdditionFIPartial(),
                 AdditionTCPartial(),
                 AdditionTCFIP(),
                 Addition_TCFI_Partial_S3(),
                 Addition_TCFI_Partial_R6$new(),
                 new('Addition_TCFI_Partial_S4'),
                 new('Addition_TCFI_Partial_RC')
)

doIt <- function(number_i_1) {
  o <- objects[[number_i_1]]
  ok <- getObjectClassKind(o)
  on <- getObjectClassNames(o)$classname

  cat('class', on, '\n')
  ro <- tryCatch(
    {
      ic <- InputContext(o, packageName_s_1 = target_package_name)
      produceManualPage(ic, pc, gc)
    },
    error = function(e) {
      cat('error', ok, on, '\n')
      NULL
    }
  )

  fn <- getObjectFunctionNames(o)
  if (length(fn) == 0L) return(list(object = ro))
  m <- sample(fn, 1L)
  # rf <- tryCatch(
  #   {
  #     ic <- InputContext(o, m, packageName_s_1 = target_package_name)
  #     produceManualPage(ic, pc, gc)
  #   },
  #   error = function(e) {
  #     cat('error method', ok, on, '\n')
  #     NULL
  #   }
  # )
  ic <- InputContext(o, m, packageName_s_1 = target_package_name)
  rf <- produceManualPage(ic, pc, gc)
  list(object = ro, fun = rf)
}

res <- lapply(seq_len(length(objects)), doIt)

test_that("produceManualPage", {
  myf <- function(r1) {
    expect_true(is.list(r1))
    expect_true(r1$context$overwritten)
    expect_true(file.exists(r1$context$filename))
    expect_output(interpretResults(r1))
  }

  myf(r1)
  myf(r2)
  myf(r3)
  myf(r4)
  myf(r5)
  lapply(res, function(e) {
    lapply(e, myf)
  })

  expect_output(produceManualPage(InputContext(NULL, packageName_s_1 = target_package_name),
                                  generationContext_o_1 = GenerationContext(verbosity_b_1 = TRUE)))
})


pc <- ProcessingContext(
  extraneous_l = list(
    'my section' = "a special dedicace to NEONIRA",
    keyword = 'documentation',
    concept = 'documentation generation'
  ),
  postProcessing_l = list(
    description = function(content_s) { content_s },
    encoding = function(content_s) { content_s }
  )
)

test_that("produceManualPage", {

  expect_output(produceManualPage(InputContext(Addition_TCFI_Partial_R6$new(), packageName_s_1 = target_package_name),
                                  pc, GenerationContext(verbosity_b_1 = TRUE)))
})
