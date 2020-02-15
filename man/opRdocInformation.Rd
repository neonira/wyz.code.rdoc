\name{opRdocInformation}
\alias{opRdocInformation}
\title{Offensive Programming R Documentation Information}
\description{list package functions and provide informations about their usage. }
\usage{
opRdocInformation()
}
\value{
A \code{data.table} with following columns
\item{name}{the object name}
\item{category}{the category of the object describe by function name. Could be
\code{CLASS}, \code{FUNCTION} or \code{DATA}.}
\item{nature}{either \code{INTERNAL} or \code{EXPORTED}.}
\item{stratum}{the stratum the object belongs to. Values are \code{CORE},
\code{LAYER_1}, \code{LAYER_2}, \code{LAYER_3}.}
\item{phasing}{main usage phase of the object. Values are \code{DESIGN},
\code{BUILD}, \code{TEST}, \code{RUN}, \code{MAINTAIN}, \code{EVOLVE} and
\code{TRANSVERSAL}.}
\item{intent}{main global intent of the object. Values are \code{PARTS_BUILDING},
\code{PARTS_ASSEMBLY}, \code{QUALITY_CONTROL}, \code{FEEDBACK}, \code{STATISTICS},
\code{CONTENT_GENERATION} and \code{UTILITIES}.}
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

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} FEEDBACK
}
\examples{
opRdocInformation()
}