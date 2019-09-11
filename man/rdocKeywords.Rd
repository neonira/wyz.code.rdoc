\name{rdocKeywords}
\alias{rdocKeywords}
\title{
\R documentation keywords
}
\description{
Provide \R documentation keywords inventory
}
\usage{
rdocKeywords(asList_b_1 = FALSE)
}
\arguments{
  \item{asList_b_1}{ a boolean to specify desired output }
}
\value{
A vector when parameter \code{asList_b_1} is \code{FALSE}, a \code{list}
otherwise.
}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing R extensions}
to know more about R documentation requirements.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\examples{
print(rdocKeywords())
print(rdocKeywords(TRUE))
}
\keyword{ keywords }
\keyword{ documentation }
