\name{ProcessingContext-class}
\docType{class}
\alias{ProcessingContext-class}
\alias{ProcessingContext}
\title{Class ProcessingContext}
\description{
Environment class ProcessingContext.
}
\usage{
ProcessingContext(extraneous_l = list(), postProcessing_l = list())
}
\arguments{
\item{extraneous_l}{An unconstrained list}
\item{postProcessing_l}{An unconstrained list}
}
\value{
An object instance of class \code{ProcessingContext}.
}
\section{Information}{
\subsection{Environment fields}{
◆ \bold{extraneous_l} list\cr◆ \bold{postProcessing_l} list\cr◆ \bold{self} environment
}
\subsection{Environment methods}{
■ \bold{verifyExtraneous}\cr■ \bold{verifyPostProcessing}
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
pc <- ProcessingContext(extraneous_l = list(), postProcessing_l = list())
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

INTENT  ▶ CONTENT_GENERATION
}
