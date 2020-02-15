computeRootPath <- function() {
  g <- getwd()

  # for Rcheck
  if (grepl('.Rcheck', g, fixed = TRUE))
    return(system.file(package = "wyz.code.rdoc"))

  # for covr - must be done prior test for testthat
  if (grepl('wyz.code.rdoc-tests/testthat', g, fixed = TRUE))
    return(system.file(package = "wyz.code.rdoc"))

  # for testthat
  if (grepl('tests/testthat', g, fixed = TRUE))
    return(normalizePath(file.path(g, '../..')))

  '.'
}
