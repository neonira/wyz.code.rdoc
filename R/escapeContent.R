escapeContent <- function(content_s_1, escapeBraces_b_1 = FALSE) {
  patchArobas <- function(x_s) {
    if (stringr::str_count(x_s, '@') == 0L) return(x_s)
    paste(strsplit(x_s, "@@|@")[[1]], collapse = '@@', sep = '@@')
  }

  patchPercent <- function(x_s) {
    if (stringr::str_count(x_s, '%') == 0L) return(x_s)
    paste(strsplit(x_s, "\\\\%|%")[[1]], collapse = '\\%',  sep = '\\%')
  }

  patchOB <- function(x_s) {
    if (stringr::str_count(x_s, '\\{') == 0L) return(x_s)
    paste(strsplit(x_s, "\\\\\\{|\\{")[[1]], collapse = '\\{',  sep = '\\{')
  }

  patchCB <- function(x_s) {
    if (stringr::str_count(x_s, '\\}') == 0L) return(x_s)
    paste(strsplit(x_s, "\\\\\\}|\\}")[[1]], collapse = '\\}', sep = '\\}')
  }

  s <- paste0(content_s_1, '\t') # to circumvent strsplit final character issue
  s <- patchArobas(s)
  s <- patchPercent(s)

  if (!escapeBraces_b_1) return(substring(s, 1L, nchar(s) - 1L))
  s <- patchOB(s)
  s <- patchCB(s)
  substring(s, 1L, nchar(s) - 1L)
}
