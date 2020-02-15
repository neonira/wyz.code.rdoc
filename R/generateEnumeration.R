generateEnumeration <- function(entries_s, itemize_b_1 = FALSE) {
  if (length(entries_s) == 0) abort('cannot enumerate without entries')
  keyword <- ifelse(itemize_b_1, 'itemize', 'enumerate')
  s <- lapply(entries_s, function(e) paste('\\item ', e))
  generateMarkup(generateParagraph(s), keyword)
}
