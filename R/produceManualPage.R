produceManualPage <- function(inputContext_o_1,
                              processingContext_o_1 = ProcessingContext(),
                              generationContext_o_1 = GenerationContext()) {
  if (generationContext_o_1$verbosity_b_1)
    cat('\n', paste(rep('-', 78), collapse = ''), '\n', sep = '')
  m <- ManualPageBuilder(inputContext_o_1, processingContext_o_1, generationContext_o_1)
  res <- m$buildManualPage()
  if (generationContext_o_1$verbosity_b_1)
    m$interpretResults(res)
  verifyDocumentationFile(res$context$filename)
  res
}
