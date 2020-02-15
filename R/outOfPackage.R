abort <- function(msg_s_1, ...) {
  stop(paste(msg_s_1, ...))
}

capitalize <- function(text_s) {
  if (missing(text_s)) return(NA_character_)
  sapply(text_s, function(e) {
    if (is.na(e)) return(e)
    if (!is.character(e)) return(e)
    n <- nchar(e)
    if (n == 0) return(e)
    if (n == 1) return(toupper(e))
    paste0(toupper(substr(e, 1, 1)), substring(e, 2))
  }, USE.NAMES = !is.null(names(text_s)))
}

catn <- function(...) cat(..., '\n')

ensureFilenameExtension <- function(filename_s_1m, extension_s_1m) {
  stopifnot(length(extension_s_1m) >= 1)
  is_valuedS <- function(x_s_1) !is.na(x_s_1) && !is.null(x_s_1) && nchar(x_s_1) > 0
  is_valued <- Vectorize(is_valuedS)

  ext <- ifelse(substr(extension_s_1m, 1, 1) == '.',
                paste0('\\', extension_s_1m),
                paste0('\\.', extension_s_1m))
  special_files <- filename_s_1m %in% c('.', '..')

  wf <- function(x_s_1) which(filename_s_1m == x_s_1)
  ifelse(special_files,
         filename_s_1m,
         {
           ex <- if (length(ext) == 1) ext[1] else ext[wf(filename_s_1m)]
           ifelse(is_valued(ex),
                  suppressWarnings(paste0(sub(paste0(ex, '$'), '', filename_s_1m, perl = TRUE),
                                          substring(ex, 2))),
                  filename_s_1m
           )
         }
  )
}

removeFilenameExtension <- function(filename_s_1m) {
  b <- stringr::str_count(filename_s_1m, '\\.')
  special_files <- grepl('^\\.+$', filename_s_1m, perl = TRUE)

  rfe <- function(fn_s_1) {
    s <- strsplit(fn_s_1, '.', fixed = TRUE)[[1]]
    paste0(s[-length(s)], collapse = '.')
  }
  vrfe <- Vectorize(rfe)

  ifelse(b != 0 & !special_files,
         vrfe(filename_s_1m),
         filename_s_1m
  )
}

guardExecution <- function(yourExpression_ex, instrumentWarnings_b = TRUE) {
  if (instrumentWarnings_b) {
    tryCatch(yourExpression_ex,
             error = function(e) e,
             warning = function(w) w)
  } else {
    tryCatch(yourExpression_ex,
             error = function(e) e)
  }
}

normalizeSpaces <- function(text_s) {
  p <- '[\\s\\b]'
  gsub(paste0('^', p, '+', '|', p, '+', '$'), '',
       gsub(paste0(p, '+'), ' ', text_s, perl = TRUE),
       perl = TRUE)
}

strBracket <- function(text_s_n) {
  paste0('[', text_s_n, ']')
}

strBrace <- function(text_s_n) {
  paste0('{', text_s_n, '}')
}

strJoin <- function(text_s, join_s_n = ', ') {
  paste(text_s, sep = '', collapse = join_s_n)
}

strParenthesis <- function(text_s_n) {
  paste0('(', text_s_n, ')')
}

buildIdentityList <- function(entries_s) {
  d <- toupper(entries_s)
  names(d) <- gsub('[^A-Z0-9_]', '', d, perl = TRUE)
  as.list(d)
}

normalizeFilename <- function(filename_s) {
  n <- nchar(filename_s)
  p <- digest::digest(filename_s)
  s <- gsub('[^a-zA-Z0-9_\\.-]', '', filename_s, perl = TRUE)
  if (n == 0 || nchar(s) != n) return(paste0(s, '_', p))
  s
}

