\name{produceManualPage}
\alias{produceManualPage}
\title{Produce Manual Page}
\description{Use this function to produce a manual page.}
\usage{
produceManualPage(inputContext_o_1,
                  processingContext_o_1 = ProcessingContext(),
                  generationContext_o_1 = GenerationContext())
}
\arguments{
\item{inputContext_o_1}{The input context object to consider for generation.
See \code{\link{InputContext}}.}
\item{processingContext_o_1}{The processing context object to consider for generation.
See \code{\link{ProcessingContext}}.}
\item{generationContext_o_1}{The generation context object to consider for generation.
See \code{\link{GenerationContext}}.}
}
\value{
A \code{list} holding generation process information.

Use function \code{\link{interpretResults}} to get knowledge of generated
parts and remaining issues.
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

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} QUALITY_CONTROL
}
\examples{
ic <- InputContext(NULL, 'append', packageName_s_1 = 'wyz.code.rdoc')

res <- produceManualPage(ic)
# WARNING: File /tmp/RtmpYIampA/append.Rd
# checkRd: (5) /tmp/RtmpYIampA/append.Rd:0-19: Must have a \\description

interpretResults(res)
# filename is /tmp/RtmpYIampA/append.Rd [OVERWRITTEN]
# generated 8 sections: name, alias, title, usage, arguments, author, keyword, encoding
# missing 3 sections: description, value, examples
# probably missing 1 section: details
}
