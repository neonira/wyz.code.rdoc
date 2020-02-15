GenerationContext <- function(targetFolder_s_1 = tempdir(),
                              overwrite_b_1 = FALSE,
                              verbosity_b_1 = FALSE,
                              useMarkers_b_1 = FALSE) {
  self <- environment()
  class(self) <- append('GenerationContext', class(self))

  if (!dir.exists(targetFolder_s_1))
    stop(targetFolder_s_1, ' must be an existing folder')

  self
}
