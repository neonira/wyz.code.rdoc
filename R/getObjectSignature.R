getObjectSignature <- function(object_o_1) {
  on <- wyz.code.offensiveProgramming::getObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)

  cn <- getObjectClassNames(object_o_1)
  fn <- guardExecution({get(cn$classname)})
  if (on == 'R6') fn <- fn$new
  if (!is.function(fn))
    abort('unable to retrieve object signature for object', strBracket(cn$classname),
          strBracket(strJoin(cn$classnames)))
  fo <- retrieveFunctionArguments(fn)
  ag <- sapply(seq_len(length(fo)), function(k) {
    paste0(names(fo[k]), ifelse(is.symbol(fo[[k]]), '',
                               paste(' =', manageSingleStrings(fo[[k]]))))
  }, simplify = FALSE )
  paste0(cn$classname, strParenthesis(strJoin(normalizeSpaces(ag), ', ')))
}
