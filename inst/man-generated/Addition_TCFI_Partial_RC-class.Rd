\name{Addition_TCFI_Partial_RC-class}
\docType{class}
\alias{Addition_TCFI_Partial_RC-class}
\alias{Addition_TCFI_Partial_RC}
\title{Class Addition_TCFI_Partial_RC}
\description{
RC class Addition_TCFI_Partial_RC.
}
\usage{
Addition_TCFI_Partial_RC(...)
}
\arguments{
\item{...}{additional arguments.}
}
\value{
An object instance of class \code{Addition_TCFI_Partial_RC}.
}
\section{Information}{
\subsection{RC class definition}{
Class Addition_TCFI_Partial_RC inherits from envRefClass.
}
\subsection{RC fields}{
◆ \bold{function_return_types} list\cr◆ \bold{label} character\cr◆ \bold{test_case_definitions} list
}
\subsection{RC methods}{
■ \bold{addInteger}\code{(x_i, y_i)}\cr■ \bold{generateError}\code{()}\cr■ \bold{generateErrorBis}\code{()}\cr■ \bold{generateWarning}\code{()}
}
\subsection{offensive programming - semantic naming}{
Class name compliance is \code{FALSE}.
}
\subsection{offensive programming - function return types}{
Class owns partial function return type instrumentation.
\tabular{rr}{\code{function_name} \tab \code{return_value}\cr

\bold{addInteger} \tab x_n \cr
\bold{generateError} \tab x_n \cr
\bold{generateWarning} \tab x_er \cr
}
}
\subsection{offensive programming - test case definitions}{
Class owns partial test case definitions.
\tabular{rr}{\code{function_name} \tab \code{recorded_tests}\cr

\bold{addInteger} \tab 9 \cr
\bold{generateError} \tab 1 \cr
\bold{generateErrorBis} \tab 1 \cr
}
}
\subsection{offensive programming - examples}{
\preformatted{library("data.table")
library("wyz.code.offensiveProgramming")

atcfiprc <- Addition_TCFI_Partial_RC(...)
tcnum <- 9 # [1,11]

# standard_R_evaluation mode
se <- EvaluationMode(defineEvaluationMode()[1])
ser <- runTestCase(atcfiprc, tcnum , se)

# type_checking_enforcement mode
tc <- EvaluationMode(defineEvaluationMode()[3])
tcr <- runTestCase(atcfiprc, tcnum, tc)}
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{class}
\encoding{UTF-8}
