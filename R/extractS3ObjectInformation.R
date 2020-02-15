extractS3ObjectInformation <- function(object_o_1) {
  on <- getObjectClassKind(object_o_1)
  stopifnot(on == 'S3')

  getFields <- function() {
    l <- ls(object_o_1)
    if (length(l) == 0) return(NULL)
    f <- sapply(l, function(e) !typeof(object_o_1[[e]]) %in% c('closure'))
    fields <- l[f]
    # if (is.null(fields) || length(fields) == 0) return(NULL) # self is mandatory field
    generatePublicFieldParagraph(object_o_1, fields)
  }

  getMethods <- function() {
    methods <- getObjectFunctionNames(object_o_1)
    if (is.null(methods) || length(methods) == 0) return(NULL)
    generatePublicMethodParagraph(object_o_1, methods)
  }

  l <- list(
    'S3 fields' = getFields(),
    'S3 methods' = getMethods()
  )

  Filter(Negate(is.null), l)
}
