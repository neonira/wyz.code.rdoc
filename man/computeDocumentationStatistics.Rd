\name{computeDocumentationStatistics}
\alias{computeDocumentationStatistics}
\title{Compute Documentation Statistics}
\description{Compute documentation statistics, providing section length in lines
and identifying too long lines.}
\usage{
computeDocumentationStatistics(filename_s_1, maxLineLength_pi_1 = 100L)
}
\arguments{
\item{filename_s_1}{A single string value}
\item{maxLineLength_pi_1}{A single positive integer value}
}
\value{
A \code{data.table} with three columns.

\item{keywords}{the section names embedded in the file}
\item{lines}{the number of lines for the section}
\item{line_length_issue}{the line numbers where issues are found or \code{NA}.}

}
\details{
Wherever a \code{line_length_issue} is not \code{NA}, you should correct the
faulty line by editing the file. Not doing so will very probably imply a failure
during check package procedure execution.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\note{
This function should be use when prior package delivery, to ensure documentation
lines meet the \R documentation specifications.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}}  LAYER_3

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}}  BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}}  STATISTICS
}
\examples{
# computeDocumentationStatistics('myfile.Rd')
}
