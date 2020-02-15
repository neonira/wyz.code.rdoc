getSectionContent <- function(filename_s_1) {

  if (!file.exists(filename_s_1)) abort(filename_s_1, 'does not exist')

  l <- readLines(filename_s_1, warn = FALSE)
  s <- paste(l, collapse = '\n')
  p <- strsplit(s, '')[[1]]
  start <- 1L
  nob <- ncb <- 0L
  sections <- vector('list', length(l))
  sk <- 1L

  section_name <- ''
  sapply(seq_len(length(p)), function(k) {
    if (p[k] == '{') {
      if (nob == 0) {
        section_name <<- stringr::str_trim(paste(p[start:(k - 1L)], collapse = ''))
        start <<- k + 1L
      }
      nob <<- nob + 1
    }

    if (p[k] == '}') ncb <<- ncb + 1

    if (nob == ncb && nob != 0) {
      sections[[sk]] <<- list(name = section_name,
                              content = paste(p[start:(k - 1L)], collapse = ''))
      sk <<- sk + 1L
      start <<- k + 1L
      nob <<- 0L
      ncb <<- 0L
    }
  })

  if (sk == 1L) return(list())
  data <- sections[1:(sk - 1L)]
  id <- sapply(data, function(e) e$name == '\\section')
  w <- which(id == TRUE)
  sapply(rev(w), function(k) {
    data[[k]]$name <<- data[[k]]$content
    data[[k]]$content <<- data[[k + 1]]$content
    data[[k + 1]] <<- NULL
  })
  Filter(Negate(is.null), data)
}
