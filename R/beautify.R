beautify <- function(escapeBraces_b_1 = FALSE) {

  generateFunction <- function(keyword_s_1) {
    function(entries_s) {
      sapply(entries_s, generateMarkup, keyword_s_1,
             escapeBraces_b_1 = escapeBraces_b_1, USE.NAMES = FALSE)
    }
  }

  candidates <- sort(c('acronym', 'bold', 'cite', 'dQuote', 'email', 'emph',
                       'enc', 'env',  'figure', 'file', 'format', 'kbd',
                       'link', 'option', 'pkg', 'preformatted', 'samp',
                       'source', 'sQuote', 'strong', 'verb', 'var', 'url',
                       'code'))

  l <- sapply(candidates, generateFunction)
  l$codelink <- function(entries) l$code(l$link(entries))
  l
}
