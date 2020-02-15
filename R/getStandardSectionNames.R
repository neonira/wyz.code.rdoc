getStandardSectionNames <- function(sort_b_1 = FALSE) {
  ic <- InputContext(FunctionParameterName('z_l'))
  v <- ic$retrieveStrategy()$strategy$section_name
  stopifnot(length(unique(v)) == length(v))
  if (sort_b_1) sort(v) else v
}