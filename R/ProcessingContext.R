ProcessingContext <- function(extraneous_l = list(),
                              postProcessing_l = list()) {
  self <- environment()
  class(self) <- append('ProcessingContext', class(self))

  verifyExtraneous <- function(extraneous_l) {
    if (is.list(extraneous_l)) return(TRUE)
    stop('extraneous_l must be a list')
  }

  verifyPostProcessing <- function(postProcessing_l) {
    if (is.list(postProcessing_l)) {
      if (all(sapply(postProcessing_l, wyz.code.offensiveProgramming::matchFunctionSignature,
                     function(content_s){}))) return(TRUE)
    }
    stop('postProcessing_l must be a list of functions taking one argument named content_s')
  }

  verifyExtraneous(extraneous_l)
  verifyPostProcessing(postProcessing_l)

  self
}