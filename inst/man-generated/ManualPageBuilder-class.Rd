\name{ManualPageBuilder-class}
\docType{class}
\alias{ManualPageBuilder-class}
\alias{ManualPageBuilder}
\title{Class ManualPageBuilder}
\description{
Environment class ManualPageBuilder.
}
\usage{
ManualPageBuilder(inputContext_o_1, processingContext_o_1 = ProcessingContext(), generationContext_o_1 = GenerationContext())
}
\arguments{
\item{inputContext_o_1}{A single object value}
\item{processingContext_o_1}{A single object value}
\item{generationContext_o_1}{A single object value}
}
\value{
An object instance of class \code{ManualPageBuilder}.
}
\section{Information}{
\subsection{Environment fields}{
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{colorizer} list\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{generationContext_o_1} environment\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{inputContext_o_1} environment\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{processingContext_o_1} environment\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{self} environment\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{strategy} list
}
\subsection{Environment methods}{
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{assembleManualPage}\code{(pieces_l)}\cr
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{buildManualPage}\code{()}\cr
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{documentContent}\code{()}\cr
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{getStrategy}\code{()}\cr
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{interpretResults}\code{(result_l)}
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
mpb <- ManualPageBuilder(inputContext_o_1, processingContext_o_1 = ProcessingContext(), generationContext_o_1 = GenerationContext())
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{
Class \code{\link{InputContext}} class \code{\link{ProcessingContext}} and class \code{\link{GenerationContext}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
