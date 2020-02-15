\name{generateSection}
\alias{generateSection}
\title{Generate Section}
\description{Generate \R documentation section}
\usage{
generateSection(sectionName_s_1, content_s)
}
\arguments{
\item{sectionName_s_1}{A single string value}
\item{content_s}{An unconstrained vector of string values}
}
\value{
A single string, containing the generated reference text. Can be appended several
times to elaborate a multiple reference text.

See references and examples below.
}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing \R extensions}
to know more about using web references in \R documentation.

Refer to \href{https://developer.r-project.org/parseRd.pdf}{Parsing Rd files} by
Duncan Murdoch.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\note{
This function should not be used directly unless you need to write your own manual
page generation program.

To generate a manual page directly, you would better use \code{\link{produceManualPage}}.
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
generateSection('concept', 'meta programming')
# "\\\\concept{meta programming}"
}
