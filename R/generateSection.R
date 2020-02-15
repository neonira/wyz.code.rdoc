generateSection <- function(sectionName_s_1, content_s) {
  rd <- rdocKeywords(TRUE)
  if (sectionName_s_1 %in% c( 'alias', rd$indices))
    return(sapply(content_s, function(e) {
      generateMarkup(e, sectionName_s_1, inline_b_1 = TRUE)
    }, USE.NAMES = FALSE))

  inl <- sectionName_s_1 %in% c('name', 'title', 'docType', 'encoding')
  if (sectionName_s_1 %in% c(rd$documentingFunctions, rd$documentingDataSets))
    return(generateMarkup(content_s, sectionName_s_1, inline_b_1 = inl))

  generateMarkup(paste0(toupper(substr(sectionName_s_1, 1L, 1L)),
                        substring(sectionName_s_1, 2L)),
                 'section', content_s, inline_b_1 = FALSE)
}
