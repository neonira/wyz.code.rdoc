\name{generateOptionLink}
\alias{generateOptionLink}
\title{Generate Option Link}
\description{Generate cross reference in \R documentation}
\usage{
generateOptionLink(options_s_1, topicName_s_1, escapeBraces_b_1 = FALSE)
}
\arguments{
\item{options_s_1}{A single string value}
\item{topicName_s_1}{A single string value}
\item{escapeBraces_b_1}{A single boolean value}
}
\value{
A single string, containing one option link. See references and examples below.
}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing \R extensions}
section 2.5, to know more about using cross references in \R documentation.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{See package link creation using function \code{\link{producePackageLink}}.}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_1

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} PARTS_BUILDING
}
\examples{
generateOptionLink('=terms.object', 'terms')
#[1] "\\\\link[=terms.object]{terms}"
}
