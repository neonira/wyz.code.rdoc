extractS4ObjectInformation <- function(object_o_1) {
  on <- getObjectClassKind(object_o_1)
  stopifnot(on == 'S4')

  class_name <- getObjectClassNames(object_o_1)$classname
  beautifier <- beautify()

  getDefinition <- function() {
    inh <- selectSuperClasses(class_name)

    c(
      if (is.null(inh)) inh else
        sentensize(paste('Class', class_name, 'inherits from', inh))
    )
  }

  getFields <- function() {
    fields <- slotNames(object_o_1)
    # slots can not be empty as it means pure virtual class
    #if (is.null(fields) || length(fields) == 0) return(NULL)
    generateParagraphCR(
      paste(documentationSymbols()$black_diamond, beautifier$bold(fields),
            sapply(fields, function(e) typeof(slot(object_o_1, e)))
      )
    )
  }

  getMethods <- function() {
    methods <- getObjectFunctionNames(object_o_1)
    if (is.null(methods) || length(methods) == 0) return(NULL)
    generatePublicMethodParagraph(object_o_1, methods)
  }

  l <- list(
    'S4 class definition' = getDefinition(),
    'S4 fields' = getFields(),
    'S4 methods' = getMethods()
  )

  Filter(Negate(is.null), l)
}
