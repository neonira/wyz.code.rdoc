\name{auditDocumentationFiles}
\alias{auditDocumentationFiles}
\title{Audit Documentation Files}
\description{Audit documentation files from a set of folders}
\usage{
auditDocumentationFiles(folder_s_1m)
}
\arguments{
\item{folder_s_1m}{An length-1 or more vector of existing folder names}
}
\value{
Provides a named \code{list} with two entries named \code{correct} and
\code{incorrect}. All entries are file names.

Incorrect entries are the ones that
has length issues as detected by function \code{\link{computeDocumentationStatistics}}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{
Function \code{\link{verifyDocumentationFile}} allows to check documentation content
using standard \R function \code{\link[tools:checkRd]{tools:checkRd}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM symbol \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_1

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}}  BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}}  QUALITY_CONTROL
}
\examples{
auditDocumentationFiles('man')
}

