\name{generateSection}
\alias{generateSection}
\title{
Generate a \R documentation section
}
\description{
Generate a \R documentation section
}
\usage{
generateSection(sectionName_s_1, content_s)
}
\arguments{
  \item{sectionName_s_1}{ the section name to consider. Must be known from
  \code{\link{rdocKeywords}}. }
  \item{content_s}{ the section textual content to consider }
}
\details{
Sections inlining is automated and managed internally. Do not bother about it.
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
