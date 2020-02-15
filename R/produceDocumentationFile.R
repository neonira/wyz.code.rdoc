produceDocumentationFile <- function(filename_s_1, content_s,
                                     generationContext_o_1) {

  fn <- file.path(generationContext_o_1$targetFolder_s_1,
                  ensureFilenameExtension(filename_s_1, '.Rd'))
  b <- generationContext_o_1$overwrite_b_1 || !file.exists(fn)
  overwritten <- FALSE
  if (b) {
    writeLines(content_s, con = fn)
    overwritten <- TRUE
    if (generationContext_o_1$verbosity_b_1) catn('wrote file', fn)
  }
  list(filename = fn, overwritten = overwritten)
}
