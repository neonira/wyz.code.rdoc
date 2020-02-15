\name{convertExamples}
\alias{convertExamples}
\title{Convert Examples}
\description{Generate example section content from \R code. }
\usage{
convertExamples(examples_l, captureOutput_b_1n = TRUE,
                mode_s_1n = c(NA, "donttest", "dontrun", "dontshow")[1])
}
\arguments{
\item{examples_l}{An unconstrained \code{list} of \code{functions}}
\item{captureOutput_b_1n}{a length-1 or N boolean vector}
\item{mode_s_1n}{An length-1 or N vector of character values taken amongst
valid values}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_3

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} RUN

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} PARTS_ASSEMBLY
}
\examples{
someComputation <- function(numberAsString_s_1) {
  suppressWarnings(sum(as.integer(strsplit(numberAsString_s_1, '')[[1]]), na.rm = TRUE))
}

examples <- list(
  function() {
    someComputation("145")
  },
  function() {
    someComputation("1547215")
  },
  function() {
    someComputation(NA_character_)
  },
  function() {
    invisible(someComputation("0x145ABC"))
  }
)

cat(convertExamples(examples, TRUE , c(NA_character_, 'donttest', 'dontrun', 'dontshow')))
# ------- example 1 -------
# someComputation("145")
# 10

# \\\\donttest{
# ------- example 2 -------
someComputation("1547215")
# 25
# }
# \\\\dontrun{
# ------- example 3 -------
# someComputation(NA)
# 0
# }
# \\dontshow{
# ------- example 4 -------
#invisible(someComputation("0x145ABC"))
# }"

}