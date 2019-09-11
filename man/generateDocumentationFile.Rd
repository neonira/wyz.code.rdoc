\name{generateDocumentationFile}
\alias{generateDocumentationFile}
\title{
Generate \R documentation file
}
\description{
Write documentation file on disk.
}
\usage{
generateDocumentationFile(filename_s_1, content_s,
                          overwrite_b_1 = FALSE, verbose_b_1 = TRUE)
}
\arguments{
  \item{filename_s_1}{ the file name to write. Might be a relative or absolute file
  path. Suffix \code{.Rd} will be added if missing.
}
  \item{content_s}{ the content to write into the file, given as a vector of strings.
}
  \item{overwrite_b_1}{
a boolean, allowing file overwriting when file already exists.
}
  \item{verbose_b_1}{
a boolean to show a message when file is overwritten.
}
}
\value{
A \code{list} with two entries named \code{filename} and \code{overwritten},
expressing the file name and the overwritten boolean flag.
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
\note{
Should be used when programming your own way to produce \R documentation.

As a end-user, no need to use this function. Use \code{\link{generateDocumentationContent}}
instead.
}
\examples{
content <- c(
  generateSection('name', 'alpha'),
  generateSection('alias', 'alpha'),
  generateSection('title', 'alpha function'),
  generateSection('description', 'Function alpha allows to ...')
)
generateDocumentationFile(file.path(tempdir(), 'myRdoc'), content, TRUE)
}
\keyword{ keywords }
\keyword{ documentation }
