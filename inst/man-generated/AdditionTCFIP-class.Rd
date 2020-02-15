\name{AdditionTCFIP-class}
\docType{class}
\alias{AdditionTCFIP-class}
\alias{AdditionTCFIP}
\title{Class AdditionTCFIP}
\description{
Environment class AdditionTCFIP.
}
\usage{
AdditionTCFIP()
}
\value{
An object instance of class \code{AdditionTCFIP}.
}
\section{Information}{
\subsection{Environment fields}{
◆ \bold{function_return_types} list\cr◆ \bold{self} environment\cr◆ \bold{test_case_definitions} list
}
\subsection{Environment methods}{
■ \bold{addDouble}\code{(x_d, y_d = 0, ...)}\cr■ \bold{addInteger}\code{(x_i, y_i)}\cr■ \bold{addNumeric}\code{(x_n, y_n)}\cr■ \bold{divideByZero}\code{(x_n)}\cr■ \bold{generateError}\code{()}\cr■ \bold{generateWarning}\code{(x_ = 8L)}
}
\subsection{offensive programming - semantic naming}{
Class name compliance is \code{TRUE}.
}
\subsection{offensive programming - function return types}{
Class owns full function return type instrumentation.
\tabular{rr}{\code{function_name} \tab \code{return_value}\cr

\bold{addNumeric} \tab x_n \cr
\bold{addDouble} \tab x_d \cr
\bold{addInteger} \tab x_i \cr
\bold{divideByZero} \tab x_d \cr
\bold{generateWarning} \tab x_w \cr
\bold{generateError} \tab x_er \cr
}
}
\subsection{offensive programming - test case definitions}{
Class owns partial test case definitions.
\tabular{rr}{\code{function_name} \tab \code{recorded_tests}\cr

\bold{addInteger} \tab 1 \cr
\bold{divideByZero} \tab 2 \cr
\bold{generateWarning} \tab 1 \cr
\bold{generateError} \tab 1 \cr
}
}
\subsection{offensive programming - examples}{
\preformatted{library("data.table")
library("wyz.code.offensiveProgramming")

atcfip <- AdditionTCFIP()
tcnum <- 5 # [1,5]

# standard_R_evaluation mode
se <- EvaluationMode(defineEvaluationMode()[1])
ser <- runTestCase(atcfip, tcnum , se)

# type_checking_enforcement mode
tc <- EvaluationMode(defineEvaluationMode()[3])
tcr <- runTestCase(atcfip, tcnum, tc)}
}
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\keyword{class}
\encoding{UTF-8}
