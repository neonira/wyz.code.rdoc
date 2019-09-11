\name{generateS3MethodSignature}
\alias{generateS3MethodSignature}
\title{
Generate S3 method signature
}
\description{
Function to create easily function signature from an S3 class
}
\usage{
generateS3MethodSignature(methodName_s_1, className_s_1, argumentNames_s)
}
\arguments{
  \item{methodName_s_1}{ the function/method name to consider }
  \item{className_s_1}{ the class name to consider}
  \item{argumentNames_s}{ the function/method argument names }
}
\value{
A single string
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
