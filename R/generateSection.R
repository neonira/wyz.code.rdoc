generateSection <- function(sectionName_s_1, content_s) {
  inl <- sectionName_s_1 %in% c('name', 'alias', 'title', 'docType', 'concept', 'keyword')
  if (sectionName_s_1 %in% rdocKeywords(TRUE)$documentingFunctions)
    return(generateContent(content_s, sectionName_s_1, inline_b_1 = inl))
  generateContent(sectionName_s_1, 'section', content_s, inline_b_1 = FALSE)
}
