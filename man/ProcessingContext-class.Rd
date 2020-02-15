\name{ProcessingContext-class}
\docType{class}
\alias{ProcessingContext-class}
\alias{ProcessingContext}
\title{Processing Context}
\description{
Environment class ProcessingContext. Defines and eases processing context
management.
}
\usage{
ProcessingContext(extraneous_l = list(), postProcessing_l = list())
}
\arguments{
\item{extraneous_l}{An unconstrained named list. Each entry will be turned into
a \R documentation section. }
\item{postProcessing_l}{An unconstrained named list. Each entry will trigger a
post processing for the related \R documentation section. }
}
\value{
An object instance of class \code{ProcessingContext}.
}
\details{
If a post processing function returns \code{NULL}, related section will be
removed from generated content. See examples below.

Post processing aims to put in action simple transformations, as changing
letter cases, or applying simple beautifying technics. See \code{\link{beautify}}.
}
\section{Information}{
\subsection{Environment fields}{
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{extraneous_l} list\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{postProcessing_l} list\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{self} environment
}
\subsection{Environment methods}{
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{verifyExtraneous}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{verifyPostProcessing}
}
\subsection{offensive programming - semantic naming}{
Class name compliance is \code{TRUE}.
}
\subsection{offensive programming - function return types}{
Class owns no function return type instrumentation.
}
\subsection{offensive programming - test case definitions}{
Class owns no test case definitions.
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{
Class \code{\link{InputContext}}, class \code{\link{GenerationContext}}
and class \code{\link{ManualPageBuilder}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CORE

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CONTENT_GENERATION
}
\examples{
pc <- ProcessingContext(
  extraneous_l = list(
    'my section' = "a special dedicace to neonira",
    keyword = 'documentation',
    concept = 'documentation generation'
  ),
  postProcessing_l = list(
    'my section' = function(content_s) {
       gsub('neonira', 'NEONIRA', content_s, fixed = TRUE)
    },
    author = function(content_s) { NULL } # destroy section
  )
)
}
