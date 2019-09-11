generateSpecialLink <- function(packageName_s_1, topicName_s_1) {
  pn <- paste0(packageName_s_1, ':', topicName_s_1)
  paste0('\\link[', pn, ']{', pn, '}')
}
