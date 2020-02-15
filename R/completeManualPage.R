completeManualPage <- function(filename_s_1, processingContext_o,
                               add_b_1 = TRUE, verbosity_b_1 = FALSE) {
  sections <- getSectionContent(filename_s_1)

  ef <- function(x_s) {
    if (substr(x_s, 1, 1) == '\\') return(substring(x_s, 2L))
    x_s
  }

  section_names <- sapply(sections, function(e) ef(e$name))
  nm <- names(processingContext_o$postProcessing_l)
  if (length(nm) == 0L) return(FALSE)

  if (add_b_1) {
    lapply(nm, function(e) {
      if (verbosity_b_1) cat('adding', e, '\n')
      sections[[e]] <<- list(name = paste0('\\', e),
                             content = processingContext_o$postProcessing_l[[e]](NULL))
    })
  } else  {
    lapply(nm, function(e) {
      w <- which(section_names == e)
      if (length(w) == 1) {
        if (verbosity_b_1) cat('patching', e, w[1], '\n')
        sections[[w[1]]] <<- list(
          name = paste0('\\', e),
          content = processingContext_o$postProcessing_l[[e]](sections[[w[1]]]$content)
        )
      }
    })
  }

  section_names <- sapply(sections, function(e) ef(e$name))
  content <- lapply(seq_len(length(sections)), function(k) {
    generateSection(section_names[k], sections[[k]]$content)
  })

  fnc <- readLines(filename_s_1, warn = FALSE)
  writeLines(c(fnc, unlist(content)), con = filename_s_1)

  TRUE
}
