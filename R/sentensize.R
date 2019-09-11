sentensize <- function(x_s_1, ...) {
  p <- normalizeSpaces(paste(x_s_1, ...))
  n <- nchar(p)
  if (n == 0) return(p)
  last <- substr(p, n, n)
  if (last != '.') p <- paste0(p, '.')
  first <- substr(p, 1, 1)
  paste0(toupper(first), substring(p, 2))
}
