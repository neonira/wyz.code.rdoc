getFunctionSignature <- function(functionName_s_1) {
  f <- tryCatch(get(functionName_s_1, mode = 'function'), error = function(e) NULL)
  if (is.null(f)) abort('no function', functionName_s_1, 'found')

  fo <- retrieveFunctionArguments(f)
  ag <- sapply(seq_len(length(fo)), function(k) {
    p <- manageSingleStrings(fo[[k]])
    paste0(names(fo[k]), if (length(p) == 0) '' else if (nchar(p) > 0) paste(' =', p) else '')
  }, simplify = FALSE)

  paste0(functionName_s_1, strParenthesis(strJoin(normalizeSpaces(ag), ', ')))
}
