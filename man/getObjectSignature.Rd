\name{getObjectSignature}
\alias{getObjectSignature}
\title{
Get Object Signature
}
\description{
Get object reification code as string
}
\usage{
getObjectSignature(object_o_1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object_o_1}{ the \R object to analyze }
}
\value{
A single string.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\examples{
source(file.path(system.file(package = "wyz.code.offensiveProgramming"), 'code-samples',
'classes', 'sample-classes.R'))
getObjectSignature(Wyx())
#[1] "Wyx(d_d, y_b_1 = FALSE)"

}
\keyword{ keywords }
\keyword{ documentation }
