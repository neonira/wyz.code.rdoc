\name{Addition_TCFI_Partial_S4-class}
\docType{class}
\alias{Addition_TCFI_Partial_S4-class}
\alias{Addition_TCFI_Partial_S4}
\title{Class Addition_TCFI_Partial_S4}
\description{
S4 class Addition_TCFI_Partial_S4.
}
\usage{
Addition_TCFI_Partial_S4(...)
}
\arguments{
\item{...}{additional arguments.}
}
\value{
An object instance of class \code{Addition_TCFI_Partial_S4}.
}
\section{Information}{
\subsection{S4 fields}{
◆ \bold{function_return_types} list\cr◆ \bold{test_case_definitions} list
}
\subsection{S4 methods}{
■ \bold{addInteger}\code{(object_o_1, x_i, y_i)}\cr■ \bold{addNumeric}\code{(object_o_1, x_d, y_d)}\cr■ \bold{generateError}\code{(object_o_1)}\cr■ \bold{generateWarning}\code{(object_o_1, x_i, y_i)}
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

atcfips <- Addition_TCFI_Partial_S4(...)
tcnum <- 3 # [1,10]

# standard_R_evaluation mode
se <- EvaluationMode(defineEvaluationMode()[1])
ser <- runTestCase(atcfips, tcnum , se)

# type_checking_enforcement mode
tc <- EvaluationMode(defineEvaluationMode()[3])
tcr <- runTestCase(atcfips, tcnum, tc)}
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{class}
\encoding{UTF-8}
