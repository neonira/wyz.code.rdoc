\name{Addition_TCFI_Partial_R6-class}
\docType{class}
\alias{Addition_TCFI_Partial_R6-class}
\alias{Addition_TCFI_Partial_R6}
\title{Class Addition_TCFI_Partial_R6}
\description{
R6 class Addition_TCFI_Partial_R6.
}
\usage{
Addition_TCFI_Partial_R6$new(...)
}
\arguments{
\item{...}{additional arguments.}
}
\value{
An object instance of class \code{Addition_TCFI_Partial_R6}.
}
\section{Information}{
\subsection{R6 class definition}{
Lock class is \code{FALSE}.
Lock objects is \code{TRUE}.
Class portability is \code{TRUE}.
}
\subsection{R6 public fields}{
◆ \bold{sum} double\cr◆ \bold{function_return_types} list\cr◆ \bold{test_case_definitions} list\cr◆ \bold{label} character
}
\subsection{R6 public methods}{
■ \bold{addInteger}\code{(x_i, y_i)}\cr■ \bold{generateError}\code{()}\cr■ \bold{generateWarning}\code{()}
}
\subsection{offensive programming - semantic naming}{
Class name compliance is \code{FALSE}.
}
\subsection{offensive programming - function return types}{
Class owns partial function return type instrumentation.
\tabular{rr}{\code{function_name} \tab \code{return_value}\cr

\bold{addInteger} \tab x_n \cr
\bold{generateError} \tab x_er \cr
}
}
\subsection{offensive programming - test case definitions}{
Class owns partial test case definitions.
\tabular{rr}{\code{function_name} \tab \code{recorded_tests}\cr

\bold{addInteger} \tab 9 \cr
\bold{generateError} \tab 1 \cr
}
}
\subsection{offensive programming - examples}{
\preformatted{library("data.table")
library("wyz.code.offensiveProgramming")

atcfipr <- Addition_TCFI_Partial_R6$new(...)
tcnum <- 8 # [1,10]

# standard_R_evaluation mode
se <- EvaluationMode(defineEvaluationMode()[1])
ser <- runTestCase(atcfipr, tcnum , se)

# type_checking_enforcement mode
tc <- EvaluationMode(defineEvaluationMode()[3])
tcr <- runTestCase(atcfipr, tcnum, tc)}
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{class}
\encoding{UTF-8}
