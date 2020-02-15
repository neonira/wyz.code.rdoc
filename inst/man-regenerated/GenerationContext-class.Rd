\name{GenerationContext-class}
\docType{class}
\alias{GenerationContext-class}
\alias{GenerationContext}
\title{Class GenerationContext}
\description{
Environment class GenerationContext.
}
\usage{
GenerationContext(targetFolder_s_1 = tempdir(), overwrite_b_1 = FALSE, verbosity_b_1 = FALSE, useMarkers_b_1 = FALSE)
}
\arguments{
\item{targetFolder_s_1}{A single string value}
\item{overwrite_b_1}{A single boolean value}
\item{verbosity_b_1}{A single boolean value}
\item{useMarkers_b_1}{A single boolean value}
}
\value{
An object instance of class \code{GenerationContext}.
}
\section{Information}{
\subsection{Environment fields}{
◆ \bold{overwrite_b_1} logical\cr◆ \bold{self} environment\cr◆ \bold{targetFolder_s_1} character\cr◆ \bold{useMarkers_b_1} logical\cr◆ \bold{verbosity_b_1} logical
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
gc <- GenerationContext(targetFolder_s_1 = tempdir(), overwrite_b_1 = FALSE, verbosity_b_1 = FALSE, useMarkers_b_1 = FALSE)
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
