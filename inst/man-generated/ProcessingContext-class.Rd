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
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{extraneous_l} list\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{postProcessing_l} list\cr
\ifelse{latex}{\out{$\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{self} environment
}
\subsection{Environment methods}{
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{verifyExtraneous}\code{(extraneous_l)}\cr
\ifelse{latex}{\out{$\leadsto$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{verifyPostProcessing}\code{(postProcessing_l)}
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
Class \code{\link{ProcessingContext}} and class \code{\link{InputContext}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
