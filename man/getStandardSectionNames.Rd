\name{getStandardSectionNames}
\alias{getStandardSectionNames}
\title{Get Standard Section Names}
\description{Get \R documentation standard section names}
\usage{
getStandardSectionNames(sort_b_1 = FALSE)
}
\arguments{
\item{sort_b_1}{A single boolean value. Result is sorted when set to  \code{TRUE}.}
}
\value{A vector of type characters, containing section names.}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing \R extensions}.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_1

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CONTENT_GENERATION
}
\examples{
getStandardSectionNames()
# [1]  "name"           "docType"        "alias"          "title"          "description"
# [6]  "usage"          "arguments"      "details"        "value"          "custom_section"
# [11] "references"     "author"         "note"           "seealso"        "examples"
# [16] "keyword"        "concept"        "encoding"       "synopsis"       "Rdversion"
# [21] "RdOpts"         "Sexpr"
}
