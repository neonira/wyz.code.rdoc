generateMarkup <- function(content_s, keyword_s_1 = NA_character_,
                           content2_s = NA_character_,
                           inline_b_1 = TRUE,
                           useSpace_b_1 = FALSE,
                           escapeBraces_b_1 = FALSE,
                           content3_s = NA_character_) {

  generateKeyword <- function(keyword_s_1) paste0('\\', keyword_s_1)
  sep <- ifelse(inline_b_1, '', '\n')
  mergeString <- function(cnt_s) paste(cnt_s, collapse = sep)

  v <- escapeContent(mergeString(content_s), escapeBraces_b_1)
  if (is.na(keyword_s_1)) return(v)
  double_content <- !is.na(content2_s[1])
  triple_content <- !is.na(content3_s[1])

  if (triple_content && !double_content)
    abort('cannot set third content withoud second content')

  required_content <- list(
    double = c('href', 'enc', 'method', 'S3method', 'S4method',
               'item', 'tabular', 'section', 'subsection', 'if',
               'newcommand', 'renewcommand', 'deqn', 'eqn'),
    triple = c('ifelse')
  )

  if (double_content) {
    se <- paste0(generateKeyword(keyword_s_1), '{')
    if (!triple_content) {
      if (!keyword_s_1 %in% required_content$double)
        abort('unautorized keyword double content', keyword_s_1)
      sp <- ifelse(keyword_s_1 %in%  c('section', 'subsection'), '\n', '')
      rv <- paste0(se, v, '}',
                   ifelse(useSpace_b_1, ' ', ''),
                   '{', sp,
                   escapeContent(mergeString(content2_s), escapeBraces_b_1),
                   sp, '}'
      )
      return(rv)
    } else {
      if (!keyword_s_1 %in% required_content$triple)
        abort('unautorized keyword triple content', keyword_s_1)

      return(paste0(se, v, '}{',
                    escapeContent(mergeString(content2_s), escapeBraces_b_1),
                    '}{',
                    escapeContent(mergeString(content3_s), escapeBraces_b_1),
                    '}'))
    }
  }

  if (!keyword_s_1 %in% union(setdiff(rdocKeywords(), unlist(required_content)), 'href'))
    abort('unknown keyword', keyword_s_1)
  se <- paste0(generateKeyword(keyword_s_1), '{', sep)
  mergeString(paste0(se,  v, sep, '}'))
}
