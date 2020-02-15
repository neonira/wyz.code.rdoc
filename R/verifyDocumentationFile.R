verifyDocumentationFile <- function(filename_s_1) {
  rv <- tryCatch(tools::checkRd(filename_s_1),
                 error = function(e) e
  )

  colorizer <- Colorizer()

  msg <- paste('File', filename_s_1)
  if (is(rv, 'error')) {
    msg <- paste('ERROR:', msg, 'is erroneous')
    cat(colorizer$error(msg), '\n')
    print(rv)
  } else {
    if (length(rv) > 0) {
      msg <- paste("WARNING:", msg)
      cat(colorizer$warning(msg), '\n')
      print(rv)
    } else {
      msg <- paste(msg, 'passes standard documentation checks')
      cat(colorizer$info(msg), '\n')
    }
  }
}
