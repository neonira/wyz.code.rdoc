\name{generateMarkup}
\alias{generateMarkup}
\title{
Generate \R documentation atomic piece
}
\description{
Generate \R documentation atomic pieces, managing various parameters to fulfil \R
documentation requirements.
}
\usage{
generateMarkup(content_s, keyword_s_1 = NA_character_,
               content2_s = NA_character_,
               inline_b_1 = TRUE, useSpace_b_1 = FALSE,
               escapeBraces_b_1 = FALSE,
               content3_s = NA_character_)
}
\arguments{
  \item{content_s}{ the content to consider }
  \item{keyword_s_1}{ a \R documentation keyword. See
                      \code{\link{rdocKeywords}.}}
  \item{content2_s}{ a second content, useful with some keywords that require
  two members}
  \item{inline_b_1}{ should the printed result be on a single line or not? }
  \item{useSpace_b_1}{ when dealing with documentation keywords that requires
  two members, some may require a space in between to work properly. This
  parameters allows you to ask for this.
}
 \item{escapeBraces_b_1}{ when \code{TRUE}, braces characters are escaped }
 \item{content3_s}{ a third content, useful with some keywords that require
  three members}
}
\details{
Very convenient function, to customize your \R documentation output.

Might be used programmatically to generate pieces or full documentation.

Tested thoroughly with one, two and three contents to cover all the markups of
\R documentation.

See examples below.
}
\value{
A single string, containing one or several lines of text. Provided content is
processed by function \code{\link{escapeContent}}.
}
\references{
Refer to
\href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing \R extensions}
to know more about R documentation requirements.
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\examples{
# 1. one content example
print(generateMarkup('a title', 'title'))
# "\\\\title{a title}"

# 2. Two contents examples
print(generateMarkup('https://neonira.github.io/offensiveProgrammingBook/',
'href', 'Offensive Programming Book'))
# "\\\\href{https://neonira.github.io/offensiveProgrammingBook/}{Offensive Programming Book}"

print(generateMarkup('a', 'item', 'description of a', useSpace_b_1 = TRUE))
# "\\\\item{a} {description of a}"

print(generateMarkup('a', 'item', 'description of a', useSpace_b_1 = FALSE))
"\\\\item{a}{description of a}"

# 3. Three contents example
print(generateMarkup('content_1', 'ifelse', 'content_2', content3_s = 'content_3'))
# "\\\\ifelse{content_1}{content_2}{content_3}"

}
\keyword{ keywords }
\keyword{ documentation }
\encoding{UTF-8}

