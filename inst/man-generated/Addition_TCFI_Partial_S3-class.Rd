\name{Addition_TCFI_Partial_S3-class}
\docType{class}
\alias{Addition_TCFI_Partial_S3-class}
\alias{Addition_TCFI_Partial_S3}
\title{Class Addition_TCFI_Partial_S3}
\description{
S3 class Addition_TCFI_Partial_S3.
}
\usage{
Addition_TCFI_Partial_S3()
}
\value{
An object instance of class \code{Addition_TCFI_Partial_S3}.
}
\section{Information}{
\subsection{S3 fields}{
◆ \bold{function_return_types} list\cr◆ \bold{label} character\cr◆ \bold{test_case_definitions} list
}
\subsection{S3 methods}{
■ \bold{addInteger}\code{(object_o_1, x_i, y_i)}\cr■ \bold{addNumeric}\code{(object_o_1, x_n, y_n)}\cr■ \bold{generateError}\code{(object_o_1)}\cr■ \bold{generateWarning2}\code{(object_o_1)}
}
\subsection{offensive programming - semantic naming}{
Class name compliance is \code{FALSE}.
}
\subsection{offensive programming - function return types}{
Class owns partial function return type instrumentation.
\tabular{rr}{\code{function_name} \tab \code{return_value}\cr

\bold{addNumeric} \tab x_n \cr
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

atcfips <- Addition_TCFI_Partial_S3()
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
