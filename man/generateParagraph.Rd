\name{generateParagraph}
\alias{generateParagraph}
\title{Generate Paragraph}
\description{
Generate paragraph, collating provided contents with given string.
}
\usage{
generateParagraph(..., collapse_s_1 = "\n", addFinalSeparator_b_1 = FALSE)
}
\arguments{
\item{...}{additional arguments, content to be collated.}
\item{collapse_s_1}{The string to be used to collate content }
\item{addFinalSeparator_b_1}{A single boolean value. When \code{TRUE}, a final
separator will be added to generated content.}
}
\value{A single string, with possibly many new line character embedded.}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{Functions \code{\link{generateParagraph2NL}} and
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
generateParagraph(LETTERS[1:3])
# "A\nB\nC"

generateParagraph(LETTERS[1:3], addFinalSeparator_b_1 = TRUE)
# "A\nB\nC\n"
}
