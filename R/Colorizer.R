Colorizer <- function() {

  colorizeText <- function(colorFunction_f_1 = crayon::blue) {
    function(text_s) {
      unlist(lapply(text_s, colorFunction_f_1))
    }
  }

  l <- lapply(list(crayon::red, crayon::yellow, crayon::green,
                   crayon::blue, crayon::magenta), colorizeText)
  names(l) <- c('error', 'warning', 'message', 'info', 'other')
  l
}
