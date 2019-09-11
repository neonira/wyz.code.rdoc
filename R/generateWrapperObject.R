generateWrapperObject <- function(fun_f_1, functionName_ch_1 = NA) {
  fn <- if (is.na(functionName_ch_1[1])) {
    deparse(substitute(fun_f_1))
  } else functionName_ch_1
  wo <- WrapperObject()
  wo$self[[fn]] <- fun_f_1
  wo
}
