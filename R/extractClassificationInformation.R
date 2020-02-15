extractClassificationInformation <- function(name_s_1, packageName_s_1) {
  ns <- getNamespace(packageName_s_1)
  exported_symbols <- getNamespaceExports(ns)
  ff <- exported_symbols[grepl(glob2rx('op*Information'), exported_symbols)][1]
  fn <- tryCatch(get(ff, asNamespace(packageName_s_1)),
                 error = function(e) NULL)
  if (!is.function(fn)) return(NULL)
  d <- fn()
  if (!data.table::is.data.table(d)) return(NULL)
  name <- NULL # nse
  info <- d[name == name_s_1]
  if (nrow(info) == 0) return(NULL)

  if (info$nature == 'INTERNAL')
    abort(name_s_1, 'is internal - Should not have manual page')

  black_right_triangle <- documentationSymbols()$black_right_triangle
  generateParagraph2NL(
    c(
      paste('STRATUM', black_right_triangle, info$stratum),
      paste('PHASING', black_right_triangle, info$phasing),
      paste('INTENT ', black_right_triangle, info$intent)
    )
  )
}
