\name{generateReference}
\alias{generateReference}
\title{
Generate \R documentation reference piece
}
\description{
Allows to generate content to be used in references \R documentation part (or
elsewhere), from a \code{list}.
}
\usage{
generateReference(data_l)
}
\arguments{
  \item{data_l}{ a \code{list} with names \code{url}, \code{label}, \code{comment}}
}
\details{
Generates a \R documentation \code{\\href} entry. The \code{url} holds the URL part,
the \code{label} holds the label part, and the comment if provided, is appended
to the result. The resulting string is appended to the string \code{'Refer to'}.
}
\value{
A single string.
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
print(generateReference(list(url = 'https://neonira.github.io/offensiveProgrammingBook/',
                             label = 'Offensive Programming Book')))
# provides following result
# "Refer to \\href{https://neonira.github.io/offensiveProgrammingBook/}{Offensive Programming Book}."
}
\keyword{ keywords }
\keyword{ documentation }