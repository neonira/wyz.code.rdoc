generatePublicFieldParagraph <- function(object_o_1, fields_s) {
  generateParagraphCR(
    paste(documentationSymbols()$black_diamond, beautify()$bold(fields_s),
          sapply(fields_s, function(e) typeof(object_o_1[[e]])))
  )
}