\name{generateTable}
\alias{generateTable}
\title{Generate Table}
\description{Generate table format in \R documentation}
\usage{
generateTable(content_dt, alignement_s_1 = NA_character_, numberRows_b_1 = FALSE)
}
\arguments{
\item{content_dt}{A data.table to be use a source data}
\item{alignement_s_1}{A single string value, expressing the column alignment
directive}
\item{numberRows_b_1}{A single boolean value. Set it when you want data rows to
be automatically numbered. }
}
\value{
a single string, containing potentially many embedded formatting strings.
}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing \R extensions}
section \code{Lists and Tables }.
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

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} PARTS_BUILDING
}
\examples{
library(data.table)
dt <- data.table::data.table(x = runif(3), y = letters[1:3])

generateTable(dt)
# "\\tabular{ll}{\n0.975343016441911 \\tab a \\cr\n
#  0.647014946676791 \\tab b \\cr\n0.576294980244711 \\tab c \\cr\n}"

generateTable(dt, numberRows_b_1 = TRUE)
# "\\tabular{rll}{\n1 \\tab 0.11690619844012 \\tab a \\cr\n
#  2 \\tab 0.467709563905373 \\tab b \\cr\n3 \\tab 0.957075224025175 \\tab c \\cr\n}"
}
