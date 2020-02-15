\name{sentensize}
\alias{sentensize}
\title{Create sentence}
\description{Create a sentence from given content}
\usage{
sentensize(x_s, ...)
}
\arguments{
\item{x_s}{An unconstrained vector of string values}
\item{...}{additional arguments (should be convertible to \code{character} type).}
}
\details{
Collate all arguments, then normalize spaces.

Finally, ensure capitalization of first letter and final colon.
}
\value{A single string.}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\note{
There is no way to ask for a different final punctuation mark. If you need to
do so, either create your own helper function or simply \code{\link{sub}}
provided result.
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
sentensize('a quick brown FOX jumps\\tover', 'the     lazy      dog')
#  "A quick brown FOX jumps over the lazy dog."
}
