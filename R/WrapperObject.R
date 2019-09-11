WrapperObject <- function() {
  self <- environment()
  class(self) <- append('WrapperObject', class(self))
  self
}
