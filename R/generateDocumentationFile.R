generateDocumentationFile <- function(filename_s_1, content_s,
                                      overwrite_b_1 = FALSE,
                                      verbose_b_1 = TRUE) {

  fn <- ensureFilenameExtension(filename_s_1, '.Rd')
  b <- overwrite_b_1 || !file.exists(fn)
  if (b) {
    writeLines(content_s, con = fn)
    if (verbose_b_1) catn('wrote file', fn)
  }
  list(filename = fn, overwritten = b)
}
