generateEnc <- function(entries_l) {
  lapply(entries_l, function(e) generateMarkup(e$text, 'enc', e$ascii))
}
