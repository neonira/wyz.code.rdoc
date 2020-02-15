manageSingleStrings <- function(anything_) {
  if (typeof(anything_) == 'language') return(as.character(as.expression(anything_)))
  l <- length(anything_)
  if (typeof(anything_) == 'integer' && l != 1 && !is.na(anything_[1]))
    return(paste0(as.character(as.expression(anything_)), 'L'))
  if (l != 1) return(as.character(anything_))
  if (is.function(anything_)) abort('functions not managed')
  if (is.character(anything_) || is.complex(anything_) || is.numeric(anything_)) {
    if (is.na(anything_)[1]) {
      if (is.character(anything_)[1]) return('NA_character_')
      if (is.numeric(anything_)[1]) {
        if (is.double(anything_)[1]) return('NA_real_')
        return('NA_integer_')
      }
      if (is.complex(anything_)[1]) return('NA_complex_')
      return('NA')
    }
  }
  if (!is.character(anything_)) {
    e <- as.character(as.expression(anything_))
    if (is.integer(anything_)) return(paste0(e, 'L'))
    return(e)
  }
  paste0('"', gsub('"', '\\"', anything_, fixed = TRUE), '"')
}
