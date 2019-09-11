\name{sentensize}
\alias{sentensize}
\title{
Sentensize a string
}
\description{
Transforms a string into sentence, enforcing upper case on first letter,
normalizing spaces, and ensuring a final point is present in the sentence.v
All spaces characters are turned to single space.
}
\usage{
sentensize(x_s_1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x_s_1}{ the string to sentensize. }
  \item{\dots}{ any complimentary string. Will be appended to x_s_1 prior processing }
}
\details{
Intented to produce well-formed sentences, human readable (from a format point of
view).
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
print(sentensize('some bla', 'bli   blo \t\n\bblu'))

# provides following result
# "Some bla bli blo blu."
}
\keyword{ keywords }
\keyword{ documentation }
