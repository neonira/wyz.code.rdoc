generateParagraph <- function(..., collapse_s_1 = '\n', addFinalSeparator_b_1 = FALSE) {
  p <- paste(..., sep = collapse_s_1, collapse = collapse_s_1)
  if (addFinalSeparator_b_1) paste0(p, collapse_s_1) else p
}

generateParagraphCR <- function(..., addFinalSeparator_b_1 = FALSE) {
  generateParagraph(..., collapse_s_1 = '\\cr\n',
                    addFinalSeparator_b_1 = addFinalSeparator_b_1)
}

generateParagraph2NL <- function(..., addFinalSeparator_b_1 = FALSE) {
  generateParagraph(..., collapse_s_1 = '\n\n',
                    addFinalSeparator_b_1 = addFinalSeparator_b_1)
}
