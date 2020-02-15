\name{GenerationContext-class}
\docType{class}
\alias{GenerationContext-class}
\alias{GenerationContext}
\title{Generation Context}
\description{
Environment class GenerationContext. Defines and eases generation context
management.
}
\usage{
GenerationContext(targetFolder_s_1 = tempdir(),
                  overwrite_b_1 = FALSE,
                  verbosity_b_1 = FALSE,
                  useMarkers_b_1 = FALSE
)
}
\arguments{
\item{targetFolder_s_1}{The target folder to store produced manual pages. Must exist.}
\item{overwrite_b_1}{A single boolean value, allowing to overwrite an existing manual page.}
\item{verbosity_b_1}{The verbosity flag. Turn on for interactive use. Keep off
for programmatic usage.}
\item{useMarkers_b_1}{A single boolean value, specifying if sections should be
generated with special markers whenever possible. }
}
\value{
An object instance of class \code{GenerationContext}.
}
\section{Information}{
\subsection{Environment fields}{
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{overwrite_b_1} logical\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{self} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{targetFolder_s_1} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{useMarkers_b_1} logical\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{verbosity_b_1} logical
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
Class \code{\link{InputContext}} class \code{\link{ProcessingContext}} class
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
GenerationContext()

GenerationContext(overwrite = TRUE, verbosity = TRUE)
}
