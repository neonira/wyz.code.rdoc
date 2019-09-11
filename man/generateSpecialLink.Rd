\name{generateSpecialLink}
\alias{generateSpecialLink}
\title{
Generate \R documentation special link
}
\description{
Will create a special \R documentation link on package name, using rare syntax
\code{\\link[packagename:topicname]{packagename:topicname}}.
}
\usage{
generateSpecialLink(packageName_s_1, topicName_s_1)
}
\arguments{
  \item{packageName_s_1}{ the package name to consider }
  \item{topicName_s_1}{ the topic name to consider }
}
\value{
A single string.
}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing R extensions}
to know more about R documentation requirements.
}
\note{
It is not possible to create such \R documentation special link, using
\code{\link{generateContent}} function.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\examples{
print(generateSpecialLink('wyz.doc.offensiveProgramming', 'runTestCase'))
}
\keyword{ keywords }
\keyword{ documentation }