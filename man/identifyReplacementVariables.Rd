\name{identifyReplacementVariables}
\alias{identifyReplacementVariables}
\title{Identify Replacement Variables}
\description{Identify replacement variables in the generated manual page to
ease their substitutions.}
\usage{
identifyReplacementVariables(filename_s)
}
\arguments{
\item{filename_s}{An unconstrained vector of string values}
}
\value{
When producing a manual page using \code{\link{produceManualPage}} function,
under format-driven mode, sections will be generated with a very simple content
based on format \code{XXX_???} to ease post processing substitutions and
hand-crafted replacements.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{ Function \code{\link{produceManualPage}} and class
\code{\link{GenerationContext}}.}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_3

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} RUN

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} FEEDBACK
}
\examples{
# identifyReplacementVariables('myfile.Rd')
}
