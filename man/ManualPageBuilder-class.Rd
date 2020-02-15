\name{ManualPageBuilder-class}
\docType{class}
\alias{ManualPageBuilder-class}
\alias{ManualPageBuilder}
\title{Manual Page Builder}
\description{
Environment class ManualPageBuilder. Creates manual pages according to the
given context.
}
\usage{
ManualPageBuilder(inputContext_o_1,
                  processingContext_o_1 = ProcessingContext(),
                  generationContext_o_1 = GenerationContext())
}
\arguments{
\item{inputContext_o_1}{The input context object to consider for generation.
See \code{\link{InputContext}}.}
\item{processingContext_o_1}{The processing context object to consider for generation.
See \code{\link{ProcessingContext}}.}
\item{generationContext_o_1}{The generation context object to consider for generation.
See \code{\link{GenerationContext}}.}
}
\value{
An object instance of class \code{ManualPageBuilder}.
}
\section{Information}{
\subsection{Environment fields}{
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{colorizer} list\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{generationContext_o_1} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{inputContext_o_1} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{processingContext_o_1} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{self} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{strategy} list
}
\subsection{Environment methods}{
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{assembleManualPage}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{buildManualPage}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{documentContent}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{getStrategy}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{interpretResults}
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
\subsection{offensive programming - examples}{
mpb <- ManualPageBuilder(inputContext_o_1,
                         processingContext_o_1 = ProcessingContext(),
                         generationContext_o_1 = GenerationContext())
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\note{
As an end-user, you may prefer to use function \code{\link{produceManualPage}} as its usage is
much more straightforward.

As a programmer, this class eases programmation of your own manual page builder. See examples below.
}
\seealso{
Class \code{\link{InputContext}} class \code{\link{ProcessingContext}} and
class  \code{\link{GenerationContext}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CORE

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} PARTS_ASSEMBLY
}
\examples{
ic <- InputContext(NULL, 'append', package = 'my.package.name')
m <- ManualPageBuilder(ic)
r <- m$buildManualPage()
interpretResults(r)
}
