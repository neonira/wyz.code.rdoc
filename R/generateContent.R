generateContent <- function(content_s, keyword_s_1 = NA, content2_s = NA,
                            inline_b_1 = TRUE,
                            useSpace_b_1 = FALSE) {

  generateKeyword <- function(keyword_s_1) paste0('\\', keyword_s_1)

  escapeContent <- function(cnt_s_1) {
    gsub('@', '@@', gsub('%', '%%', cnt_s_1, fixed = TRUE), fixed = TRUE)
  }

  sep <- ifelse(inline_b_1, '', '\n')
  mergeString <- function(cnt_s) paste(cnt_s, collapse = sep)

  v <- escapeContent(mergeString(content_s))
  if (is.na(keyword_s_1)) return(v)
  double_content <- !is.na(content2_s[1])

  if (!double_content) {
    se <- paste0(generateKeyword(keyword_s_1), '{', sep)
    if (!keyword_s_1 %in% rdocKeywords()) abort('unknown keyword', keyword_s_1)
    #if (!grepl('\n', v, fixed = TRUE)) return(paste0(se, v, '}')) #one-liner
    mergeString(paste0(se,  v, sep, '}'))
  } else {
    if (!keyword_s_1 %in% c('href', 'item', 'section')) abort('unautorized keyword', keyword_s_1)
    se <- paste0(generateKeyword(keyword_s_1), '{')
    sp <- ifelse(keyword_s_1 == 'section', '\n', '')
    paste0(se, v, '}',
           ifelse(useSpace_b_1, ' ', ''),
           '{', sp,
           escapeContent(mergeString(content2_s)),
           sp, '}'
    )
  }
}
