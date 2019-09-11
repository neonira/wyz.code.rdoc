generateLabel <- function(identifier_s_1) {
  tolower(gsub('([A-Z]+)', ' \\1', identifier_s_1, perl = TRUE))
}
