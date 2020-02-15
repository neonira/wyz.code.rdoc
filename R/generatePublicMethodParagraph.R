generatePublicMethodParagraph <- function(object_o_1, methods_s) {
  beautifier <- beautify()
  sig <- sapply(methods_s, function(e) {
    s <- getObjectMethodSignature(object_o_1, e)
    r <- regexpr('(', s, fixed = TRUE)
    paste0(beautifier$bold(substr(s, 1, r[1] - 1)), beautifier$code(substring(s, r[1])))
  })
  generateParagraphCR(paste(documentationSymbols()$black_square, sig))
}
