extractRCObjectInformation <- function(object_o_1) {
  on <- getObjectClassKind(object_o_1)
  stopifnot(on == 'RC')

  class_name <- getObjectClassNames(object_o_1)$classname

  getDefinition <- function() {
    inh <- selectSuperClasses(class_name)

    c(
      if (is.null(inh)) inh else
        sentensize(paste('Class', class_name, 'inherits from', inh))
    )
  }

  getFields <- function() {
    l <- ls(object_o_1)
    if (length(l) == 0) return(NULL)
    f <- sapply(l, function(e) !typeof(object_o_1[[e]]) %in% c('closure'))
    if (all(f == FALSE)) return(NULL)
    fields <- l[f]
    generatePublicFieldParagraph(object_o_1, fields)
  }

  getMethods <- function() {
    methods <- getObjectFunctionNames(object_o_1)
    if (is.null(methods) || length(methods) == 0) return(NULL)
    generatePublicMethodParagraph(object_o_1, methods)
  }

  l <- list(
    'RC class definition' = getDefinition(),
    'RC fields' = getFields(),
    'RC methods' = getMethods()
  )

  Filter(Negate(is.null), l)
}
