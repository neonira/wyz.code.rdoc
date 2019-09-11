\name{generateDocumentationContent}
\alias{generateDocumentationContent}
\title{
Generate \R documentation file.
}
\description{
Generate a \R documentation file from an offensive programming instrumented object.
}
\usage{
generateDocumentationContent(targetFolder_s_1, kind_s_1, name_s_1,
                             object_o_1, packageName_s_1, extraneous_l = list(),
                             typeFactory_o_1 = FunctionParameterTypeFactory(),
                             overwrite_b_1 = FALSE)
}

\arguments{
  \item{targetFolder_s_1}{ the target folder to write files to }
  \item{kind_s_1}{ a single string taken amongst \code{'package'}, \code{'class'},
  \code{'method'}.
}
  \item{name_s_1}{
Either the package name, the class name or the method name, depending on previous
argument.
}
  \item{object_o_1}{
an object of the given class
}
  \item{packageName_s_1}{
the name of the targeted package to produce documentation for
}
  \item{extraneous_l}{
a list of extraneous data, to consider when producing \R documentation content.
}
  \item{typeFactory_o_1}{
a type factory object. See \code{\link[wyz.code.offensiveProgramming]{FunctionParameterTypeFactory}}
}
  \item{overwrite_b_1}{
a boolean, allowing file overwriting when file already exists.
}
}
\value{
A \code{list} with two entries named \code{filename} and \code{overwritten},
expressing the filename and the overwritten boolean flag.

If a single file is produced, then a such \code{list} is returned, otherwise,
a \code{list} of a such \code{list} is returned.
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
Allows to produce on-demand, 3 kinds of \R documentation files, according to
passed parameters.

To produce, all the methods files, in a single shot, from the provided \R object,
simply pass \code{NA} as parameter \code{name_s_1} and
\code{'method'} as parameter \code{kind_s_1}.
}
\examples{
# see system.file('inst/ut-generateDocumentationFiles.R', package = 'wyz.code.rdoc')
# for a complete example
}
\keyword{ keywords }
\keyword{ documentation }
