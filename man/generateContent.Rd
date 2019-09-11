\name{generateContent}
\alias{generateContent}
\title{
Generate \R documentation content
}
\description{
Generate a \R documentation atomic piece
}
\usage{
generateContent(content_s, keyword_s_1 = NA, content2_s = NA,
                inline_b_1 = TRUE, useSpace_b_1 = FALSE)
}
\arguments{
  \item{content_s}{ the content to consider }
  \item{keyword_s_1}{ a \R documentation keyword. See \code{\link{rdocKeywords}.}}
  \item{content2_s}{ a second content, useful with some keywords that require
  two members}
  \item{inline_b_1}{ should the printed result be inline or not ? }
  \item{useSpace_b_1}{ when dealing with documentation keywords that requires two
  members, some may require a space in between to work properly. This parameters
  allows you to ask for this.
}
}
\details{
Very convenient function, to customize your \R documentation output.

Might be used programmatically to generate pieces or full documentation.

See examples below.
}
\value{
A string, containing one or several lines of text.
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
print(generateContent('a title', 'title'))
print(generateContent('https://neonira.github.io/offensiveProgrammingBook/', 'href',
'Offensive Programming Book'))
print(generateContent('a', 'item', 'description of a', useSpace_b_1 = TRUE))
print(generateContent('a', 'item', 'description of a', useSpace_b_1 = FALSE))
}
\keyword{ keywords }
\keyword{ documentation }
