generateOptionSexpr <- function(options_s_1, topicName_s_1,
                                escapeBraces_b_1 = FALSE) {
  paste0('\\Sexpr[', options_s_1, ']{',
         escapeContent(topicName_s_1, escapeBraces_b_1), '}')
}

generateOptionLink <- function(options_s_1, topicName_s_1,
                               escapeBraces_b_1 = FALSE) {
  paste0('\\link[', options_s_1, ']{',
         escapeContent(topicName_s_1, escapeBraces_b_1), '}')
}

producePackageLink <- function(packageName_s_1, topicName_s_1) {
  pn <- paste0(packageName_s_1, ':', topicName_s_1)
  generateOptionLink(pn, pn)
}
