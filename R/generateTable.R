generateTable <- function(content_dt, alignement_s_1 = NA_character_,
                          numberRows_b_1 = FALSE) {
  nc <- ncol(content_dt)
  if (nc == 0) abort('cannot build table without columns')

  nr <- nrow(content_dt)
  if (nr == 0) abort('cannot build table without rows')

  al <- if (is.na(alignement_s_1)) paste(rep('l', nc), collapse = '') else tolower(alignement_s_1)
  ss <- strsplit(al, '')[[1]]
  if (length(ss) != nc) abort(al, 'is length', length(ss), 'is wrong. Should be', nc)
  v <- setdiff(unique(ss), c('l', 'r', 'c'))
  if (length(v) > 0) abort(al, 'is a wrong alignement specification. Use only l, c and r, please.')

  if (numberRows_b_1) al <- paste0('r', al)

  header <- paste0(paste(colnames(content_dt), collapse = ' \\tab '), '\\cr\n')

  w <- sapply(seq_len(nr), function(k) {
    s <- sapply(seq_len(nc), function(j) {
      paste0(ifelse(k == 1 && j == 1, '\n', ''),
             ifelse(numberRows_b_1 && j == 1, paste0(k, ' \\tab '), ''),
             as.character(content_dt[k, j, with = FALSE]))
    })
    paste(generateParagraph(s, collapse_s_1 = ' \\tab '), '\\cr\n')
  })
  generateMarkup(al, 'tabular', c(header, w))
}
