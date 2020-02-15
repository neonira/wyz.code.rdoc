\name{verifyDocumentationFile}
\alias{verifyDocumentationFile}
\title{Verify Documentation File}
\description{Verify documentation file compliance to \R documentation scheme.}
\usage{
verifyDocumentationFile(filename_s_1)
}
\arguments{
\item{filename_s_1}{A single string value}
}
\value{
Echoes on \code{stdout} status of documentation verification, as done by
\code{\link[tools:checkRd]{tools:checkRd}}.
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

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} RUN

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} QUALITY_CONTROL
}
\examples{
# verifyDocumentationFile("myfile.Rd")
}
