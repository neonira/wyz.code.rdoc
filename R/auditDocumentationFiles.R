auditDocumentationFiles <- function(folder_s_1m) {
  lf <- list.files(folder_s_1m, utils::glob2rx("*.Rd"), full.names = TRUE)
  w <- sapply(lf, function(e) {
    h <- computeDocumentationStatistics(e)
    f <- Filter(Negate(is.na), h$line_length_issue)
    if (length(f) > 0L)  e else NA_character_
  })

  list(
    correct = names(Filter(is.na, w)),
    incorrect = names(Filter(Negate(is.na), w))
  )
}
