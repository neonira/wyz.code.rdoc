getObjectConstructor <- function(object_o_1) {
  on <- wyz.code.offensiveProgramming::getObjectClassKind(object_o_1)
  if (is.na(on[1]) || on[1] == 'unknown') return(NA)

  cn <- getObjectClassNames(object_o_1)
  fn <- guardExecution({get(cn$classname)})
  b <- TRUE
  if (on == 'R6') {
    b <- is.function(object_o_1$initialize)
    fn <- if (b) object_o_1$initialize else fn$new
  }
  if (!is.function(fn))
    abort('unable to retrieve object signature for object', strBracket(cn$classname),
          strBracket(strJoin(cn$classnames)))
  list(function_f = fn, signature_flag = !b, on = on, classname = cn$classname)
}
