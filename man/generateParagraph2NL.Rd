\name{generateParagraph2NL}
\alias{generateParagraph2NL}
\title{Function generateParagraph2NL}
\description{
Generate paragraph, collating provided contents with double new line.
}
\usage{
generateParagraph2NL(..., addFinalSeparator_b_1 = FALSE)
}
\arguments{
\item{...}{additional arguments.}
\item{addFinalSeparator_b_1}{A single boolean value}
}
\value{A single string, with possibly many new line character embedded.}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{Functions \code{\link{generateParagraph}} and
\code{\link{generateParagraphCR}}.}
\keyword{function}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_1

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} PARTS_BUILDING
}
\examples{
generateParagraph2NL(LETTERS[1:3])
# "A\n\nB\n\nC"

generateParagraph2NL(LETTERS[1:3], addFinalSeparator_b_1 = TRUE)
# "A\n\nB\n\nC\n\n"
}
