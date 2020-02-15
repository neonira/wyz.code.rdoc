\name{generateEnumeration}
\alias{generateEnumeration}
\title{Generate Enumeration}
\description{Generate enumeration for \R documentation}
\usage{
generateEnumeration(entries_s, itemize_b_1 = FALSE)
}
\arguments{
\item{entries_s}{An unconstrained vector of string values}
\item{itemize_b_1}{A single boolean value}
}
\value{
a special character vector to mimic either \code{enumerate} or \code{itemize}
accordingly to \R documentation specification.
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
generateEnumeration(LETTERS[1:3])
# [1] "\\enumerate{\\item  A\n\\item  B\n\\item  C}"

generateEnumeration(LETTERS[1:3], TRUE)
# [1] "\\itemize{\\item  A\n\\item  B\n\\item  C}"
}
