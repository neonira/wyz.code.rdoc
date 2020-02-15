computeDocumentationStatistics <- function(filename_s_1, maxLineLength_pi_1 = 100L) {
  sections <- getSectionContent(filename_s_1)

  data.table::data.table(
    keywords = sapply(sections, function(e) regmatches(e$name, gregexpr('[a-zA-Z]+', e$name))),
    lines = sapply(sections, function(e) {
      l <- stringr::str_count(e$content, '\n')
      ifelse(l, l - 1L, 1L)
    }),
    line_length_issue = sapply(sections, function(e) {
      l <- strsplit(e$content, '\n')[[1]]
      n <- nchar(l)
      w <- which(n > maxLineLength_pi_1)
      if (length(w) == 0) NA_character_ else paste(w - 1L, collapse = ', ')
    })
  )
}
