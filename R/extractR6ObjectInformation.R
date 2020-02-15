extractR6ObjectInformation <- function(object_o_1) {
  on <- getObjectClassKind(object_o_1)
  stopifnot(on == 'R6')

  class_name <- getObjectClassNames(object_o_1)$classname
  generator <- eval(parse(text = class_name))

  beautifier <- beautify()

  getDefinition <- function() {
    inh <- generator$get_inherit()
    lock_class <- generator$lock_class
    lock_objects <- generator$lock_objects
    portability <- generator$portable

    c(
      if (is.null(inh)) inh else
        sentensize(paste('Class', class_name, 'inherits from', inh)),
      sentensize('Lock class is', beautifier$code(lock_class)),
      sentensize('Lock objects is', beautifier$code(lock_objects)),
      sentensize('Class portability is', beautifier$code(portability))
    )
  }

  getPublicFields <- function() {
    fields <- generator$public_fields
    if (is.null(fields)) return(fields)
    generatePublicFieldParagraph(object_o_1, names(fields))
  }

  getPublicMethods <- function() {
    methods <- getObjectFunctionNames(object_o_1)
    if (is.null(methods) || length(methods) == 0) return(NULL)
    generatePublicMethodParagraph(object_o_1, methods)
  }

  getActiveBindings <- function() {
    active_bindings <- generator$active
    if (is.null(active_bindings)) return(active_bindings)
    generateParagraphCR(
      paste(documentationSymbols()$bulls_eye, beautifier$bold(names(active_bindings)))
    )
  }

  l <- list(
    'R6 class definition' = getDefinition(),
    'R6 public fields' = getPublicFields(),
    'R6 public methods' = getPublicMethods(),
    'R6 active bindings' =  getActiveBindings()
  )

  Filter(Negate(is.null), l)
}