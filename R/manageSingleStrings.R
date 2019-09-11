manageSingleStrings <- function(anything_) {
  if (typeof(anything_) == 'language') return(as.character(as.expression(anything_)))
  if (typeof(anything_) == 'integer') return(paste0(as.character(as.expression(anything_)), 'L'))
  if (length(anything_) != 1) return(anything_)
  if (!is.character(anything_)) return(anything_)
  paste0('"', gsub('"', '\\"', anything_, fixed = TRUE), '"')
}
