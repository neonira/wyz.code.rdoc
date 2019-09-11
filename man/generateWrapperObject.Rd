\name{generateWrapperObject}
\alias{generateWrapperObject}
\title{Generate wrapper object.}
\description{
Use method \code{generateWrapperObject} to generate a wrapper object.
}
\usage{
generateWrapperObject(fun_f_1, functionName_ch_1 = NA)
}
\arguments{
\item{fun_f_1}{A 1-length vector holding a function, to be encapsulated into the
object. }
\item{functionName_ch_1}{A 1-length vector of character values, expressing the
function name.}
}
\value{
An object of class \code{\link{WrapperObject}} that could be used to easily
generate manual pages using \code{wyz.code.rdoc}.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\examples{
# typical case
myfun <- function(x_i, y_i) { x_i + y_i}
o <- generateWrapperObject(myfun)
print(o)
# <environment: 0x0000021d7abd9710>
# attr(,"class")
# [1] "WrapperObject" "environment"
ls(o)
# [1] "removeEllipsisName" "self"
}
