\name{escapeContent}
\alias{escapeContent}
\title{Escape Specials Characters In Content}
\description{Manage characters to be escaped in \R documentation text}
\usage{
escapeContent(content_s_1, escapeBraces_b_1 = FALSE)
}
\arguments{
\item{content_s_1}{A single string value that is the content to consider}
\item{escapeBraces_b_1}{A single boolean value, allowing to escape braces also}
}
\value{
A single \code{string} with character '@' and '\%' escaped.

When \code{escapeBraces_b_1} is set, characters '\{' and '\}' are also escaped.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}}  LAYER_1

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CONTENT_GENERATION
}
\examples{
escapeContent('www@xxx.com')
# "www@@xxx.com"

escapeContent('\\\\code{ x \%\% y }', TRUE)
# "\\\\code\\{ x \\%\\% y \\}"
}
