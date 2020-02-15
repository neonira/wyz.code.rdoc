\name{produceDocumentationFile}
\alias{produceDocumentationFile}
\title{Produce Documentation File}
\description{Use this function to save documentation text into a documentation
file.}
\usage{
produceDocumentationFile(filename_s_1, content_s, generationContext_o_1)
}
\arguments{
\item{filename_s_1}{the target file name to use}
\item{content_s}{An unconstrained vector of string values}
\item{generationContext_o_1}{The generation context object to consider for generation.
See \code{\link{GenerationContext}}.}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\note{
From a end-user perspective, this function should only be used indirectly through a
call to \code{\link{produceManualPage}} function.

Direct call is meaningful when crafting your own manual page builder code/program.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_1

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CONTENT_GENERATION
}
\examples{
fn <- tempfile()
p <- produceDocumentationFile(basename(fn), c(
  generateSection('name', 'alpha'),
  generateSection('alias', 'alpha'),
  generateSection('keyword', 'documentation generation')
),
GenerationContext(dirname(fn)))

p
# $filename
# [1] "/tmp/RtmpSWZq4H/filee3c2700207f.Rd"
#
# $overwritten
# [1] TRUE

readLines(p$filename)
# [1] "\\name{alpha}" "\\alias{alpha}" "\\keyword{documentation generation}"

}
