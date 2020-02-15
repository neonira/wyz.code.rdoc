identifyReplacementVariables <- function(filename_s) {
  l <- lapply(filename_s, function(f) {
    if (!file.exists(f)) abort(paste(f, 'is not an existing file'))
    r <- readLines(f, warn = FALSE)
    unlist(
      Filter(function(e) length(e) > 0,
             regmatches(r, gregexpr('XXX_[\\d]{3}', r, perl = TRUE)))
    )
  })
  names(l) <- filename_s
  l
}