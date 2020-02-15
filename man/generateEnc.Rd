\name{generateEnc}
\alias{generateEnc}
\title{Generate Enc}
\description{
Generate special markup for encoding text in \R documentation
}
\usage{
generateEnc(entries_l)
}
\arguments{
\item{entries_l}{An unconstrained named \code{list} of \code{lists} with \code{text} and
\code{ascii} names, holding respectively international text and \acronym{ASCII} equivalent.}
}
\value{A \code{list} of \code{character} vector}
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
generateEnc(list(list(text = 'Français', ascii = 'Francais')))
# [[1]]
# [1] "\\enc{Français}{Francais}"

generateEnc(list(list(text = 'é', ascii = 'e'), list(text = 'è', ascii = 'e')))
# [[1]]
# [1] "\\enc{é}{e}"

# [[2]]
# [1] "\\enc{è}{e}"
}
