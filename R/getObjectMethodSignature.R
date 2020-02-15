getObjectMethodSignature <- function(object_o_1, methodName_s_1) {

  if (!is.object(object_o_1)) abort('parameter named object_o_1 is not an object')

  v <- getObjectClassKind(object_o_1)
  supported <- c('environment', 'S3', 'R6', 'S4', 'RC')
  if (!v %in% supported)
    abort(paste('parameter object_o_1 is not of supported type',
                strBracket(v), 'supported types are', strBracket(strJoin(supported))))

  cn <- getObjectClassNames(object_o_1)$classname

  fn <- switch(v,
               'S4' = selectMethod(methodName_s_1, cn),
               'S3' = get(paste0(methodName_s_1, ".", cn)),
               'RC' = object_o_1$getRefClass()$def@refMethods[[methodName_s_1]],
               object_o_1[[methodName_s_1]]
  )

  if (!is.function(fn))
    abort(paste('method', methodName_s_1, 'does not exist in provided', v, 'object of class', cn))

  fo <- retrieveFunctionArguments(fn)
  ag <- sapply(seq_len(length(fo)), function(k) {
    paste0(names(fo[k]), ifelse(is.symbol(fo[[k]]), '',
                                paste(' =', manageSingleStrings(fo[[k]]))))
  }, simplify = FALSE )

  args <- strParenthesis(strJoin(normalizeSpaces(ag), ', '))
  # 'name' is a formal generic function; S3 methods will not likely be found ==> suppressWarnings
  # if (suppressWarnings(isS3method(f = methodName_s_1, class = cn))) {
  #   generateS3MethodSignature(methodName_s_1, cn, args)
  # } else
  # WARNING: File /tmp/rdoc/Bu_S3-class.Rd
  # checkRd: (7) /tmp/rdoc/Bu_S3-class.Rd:23: Tag \method not valid outside a code block
  paste0(methodName_s_1, args)
}
