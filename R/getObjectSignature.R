getObjectSignature <- function(object_o_1) {
  goc <- getObjectConstructor(object_o_1)
  if (is.na(goc[1])) return(NA)

  fo <- retrieveFunctionArguments(goc$function_f)
  ag <- sapply(seq_len(length(fo)), function(k) {
    paste0(names(fo[k]), ifelse(is.symbol(fo[[k]]), '',
                                paste(' =', manageSingleStrings(fo[[k]]))))
  }, simplify = FALSE )

  args <- if (goc$on == 'R6' && !goc$signature) '()' else strParenthesis(strJoin(normalizeSpaces(ag), ', '))
  paste0(paste0(goc$classname, ifelse(goc$on == 'R6', '$new', '')), args)
}
