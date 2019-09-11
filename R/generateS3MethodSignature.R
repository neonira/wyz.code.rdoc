generateS3MethodSignature <- function(methodName_s_1, className_s_1, argumentNames_s) {
  args <- if (length(argumentNames_s) == 0) '' else paste(argumentNames_s, collapse = ', ')
  paste0('\\method{', methodName_s_1, '}{', className_s_1, '}(', args, ')')
}