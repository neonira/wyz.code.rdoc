convertExamples <- function(examples_l, captureOutput_b_1n = TRUE,
                            mode_s_1n = c(NA_character_, 'donttest',
                                          'dontrun', 'dontshow')[1]) {

  generateExamples <- function(data_l, mode_s = NA_character_) {
    ld <- length(data_l)
    lm <- length(mode_s)
    b <- lm > 1
    if (b) stopifnot(lm == ld)
    sd <- setdiff(unique(mode_s), c(NA_character_, 'donttest', 'dontrun', 'dontshow'))
    stopifnot(length(sd) == 0)
    unlist(lapply(seq_len(ld), function(k) {
      if (is.na(mode_s[ifelse(b, k, 1)])) {
        paste(generateMarkup(data_l[[k]], inline_b_1 = FALSE), '\n')
      } else {
        generateMarkup(data_l[[k]], mode_s[ifelse(b, k, 1)], inline_b_1 = FALSE)
      }
    }))
  }

  generateExamplesTrace <- function(examples_l, captureOutput_b_1n = TRUE) {
    if (!is.list(examples_l)) stop('examples_l must be a list of functions')
    b <- sapply(examples_l, wyz.code.offensiveProgramming::matchFunctionSignature)
    if (!all(b)) stop('examples_l must be a list of functions. Erroneous entries ',
                      strBrace(which(b == FALSE)))

    s <- paste(rep('-', 7), collapse = '')
    len <- length(captureOutput_b_1n)
    lapply(seq_len(length(examples_l)), function(k) {
      b <- if (len == 1) captureOutput_b_1n else captureOutput_b_1n[k]
      v <- vector('character', 2L + ifelse(b, 1, 0))
      v[1] <- paste('#', s, 'example', k, s)
      v[2] <- as.character(body(examples_l[[k]])[-1])
      if (b)
        v[3] <- paste('#', paste0(examples_l[[k]](), collapse = ', '))
      v
    })
  }

  generateParagraph(
    generateExamples(
      generateExamplesTrace(examples_l, captureOutput_b_1n),
      mode_s_1n
    )
  )
}
