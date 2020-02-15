extractEnvObjectInformation <- function(object_o_1) {
  on <- getObjectClassKind(object_o_1)
  stopifnot(on == 'environment')

  getPublicFields <- function() {
    l <- ls(object_o_1)
    f <- sapply(l, function(e) !typeof(object_o_1[[e]]) %in% c('closure'))
    generatePublicFieldParagraph(object_o_1, l[f])
  }

  getPublicMethods <- function() {
    methods <- getObjectFunctionNames(object_o_1)
    if (is.null(methods) || length(methods) == 0) return(NULL)
    generatePublicMethodParagraph(object_o_1, methods)
  }

  l <- list(
    'Environment fields' = getPublicFields(),
    'Environment methods' = getPublicMethods()
  )

  Filter(Negate(is.null), l)
}