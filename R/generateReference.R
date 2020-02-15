generateReference <- function(data_l) {
  sentensize(paste('Refer to',
                   generateMarkup(data_l$url, 'href', data_l$label),
                   ifelse('comment' %in% names(data_l), data_l$comment, '')))
}
