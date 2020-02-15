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
◆ \bold{colorizer} list\cr◆ \bold{generationContext_o_1} environment\cr◆ \bold{inputContext_o_1} environment\cr◆ \bold{processingContext_o_1} environment\cr◆ \bold{self} environment\cr◆ \bold{strategy} list
}
\subsection{Environment methods}{
■ \bold{assembleManualPage}\cr■ \bold{buildManualPage}\cr■ \bold{documentContent}\cr■ \bold{getStrategy}\cr■ \bold{interpretResults}
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
Class \code{\link{InputContext}} class \code{\link{ProcessingContext}} class \code{\link{GenerationContext}} and class \code{\link{ManualPageBuilder}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM ▶ CORE

PHASING ▶ BUILD

INTENT  ▶ PARTS_ASSEMBLY
}
