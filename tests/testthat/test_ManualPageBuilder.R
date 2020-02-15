context("ManualPageBuilder")

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

td <- tempdir() # 'tmp'
if (!dir.exists(td)) dir.create(td)

verbose <- FALSE

generation_contexts <- list(
  without_markers = GenerationContext(targetFolder_s_1 = td,
                                      overwrite_b_1 = TRUE,
                                      verbosity_b_1 = verbose),

  with_markers = GenerationContext(targetFolder_s_1 = td,
                                   useMarkers_b_1 = TRUE,
                                   overwrite_b_1 = TRUE,
                                   verbosity_b_1 = verbose)
)

processing_contexts <- list(
  standard_context = ProcessingContext(),
  with_extraneous = ProcessingContext(
    extraneous_l  = list(
      details = sentensize('some blabla for details')
    )
  ),
  with_examples = ProcessingContext(
    extraneous_l  = list(
      examples = convertExamples(list(
        function() { runif(3) },
        function() { 'test'},
        function() { seq_len(7) }
      ),
      c(FALSE, FALSE, TRUE),
      c(NA_character_, 'dontrun', 'donttest'))
    )
  ),
  with_postProcessing = ProcessingContext(
    postProcessing_l = list(
      examples = function(content_s) {
        generateParagraph(c( 'runif(3)', 'test', 'seq_len(7)' ))
      }
    )
  )
)


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

buildPage <- function(icc, pc, gc) {
  m <- ManualPageBuilder(icc,  pc, gc)
  m$buildManualPage()
}

doIt <- function(number_i_1) {
  np <- names(processing_contexts)
  ng <- names(generation_contexts)

  lapply(seq_len(length(np)), function(p) {
    lapply(seq_len(length(ng)), function(g) {
      o <- objects[[number_i_1]]
      ok <- getObjectClassKind(o)
      on <- getObjectClassNames(o)$classname
      tryCatch(
        {
          ic <- InputContext(o)
          rv <- buildPage(ic, processing_contexts[[p]], generation_contexts[[g]])
          rv$renamed <- file.path(generation_contexts[[g]]$targetFolder_s_1,
                                  sprintf('%s-%s-%02d-%02d.Rd', ok, on, p, g))
          file.rename(rv$context$filename, rv$renamed)
          rv
        },
        error = function(e) cat('error', ok, on, p, g, e$message, '\n')
      )
    })
  })
}

res <- lapply(seq_len(length(objects)), doIt)

test_that("ManualPageBuilder", {
  expect_length(res, 22)
  expect_true(all(sapply(res, is.list)))
  lapply(res, function(r) {
    lapply(r, function(p) {
      lapply(p, function(z) {
        expect_true(exists('renamed', z))
      })
    })
  })

})


options('rdhoc_classification' = 'wyz.code.offensiveProgramming')
test_that("ManualPageBuilder", {
  expect_true(is.list(buildPage(InputContext(NULL, 'runFunction'),
                                ProcessingContext(), generation_contexts$without_markers)))


})



