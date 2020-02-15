\name{InputContext-class}
\docType{class}
\alias{InputContext-class}
\alias{InputContext}
\title{Class InputContext}
\description{
Environment class InputContext.
}
\usage{
InputContext(object_o_1, methodName_s_1 = NA_character_, packageName_s_1 = NA_character_, dataFilename_s_1 = NA_character_
}
\arguments{
\item{object_o_1}{A single object value}
\item{methodName_s_1}{A single string value}
\item{packageName_s_1}{A single string value}
\item{dataFilename_s_1}{A single string value}
}
\value{
An object instance of class \code{InputContext}.
}
\section{Information}{
\subsection{Environment fields}{
◆ \bold{beautifier} list\cr◆ \bold{class_kind} character\cr◆ \bold{class_name} character\cr◆ \bold{data_name} NULL\cr◆ \bold{dataFilename_s_1} character\cr◆ \bold{file_name} character\cr◆ \bold{hack_description} logical\cr◆ \bold{instrumentationLevel} list\cr◆ \bold{kind} double\cr◆ \bold{kinds} character\cr◆ \bold{methodName_s_1} character\cr◆ \bold{number_replacements} integer\cr◆ \bold{object_o_1} list\cr◆ \bold{packageName_s_1} character\cr◆ \bold{self} environment\cr◆ \bold{typeFactory_o_1} environment\cr◆ \bold{use_markers} logical
}
\subsection{Environment methods}{
■ \bold{buildMethodName}\cr■ \bold{generateConditionalMarker}\cr■ \bold{generateConditionalMarkerGenerator}\cr■ \bold{getFilename}\cr■ \bold{getKind}\cr■ \bold{getName}\cr■ \bold{markerGenerator}\cr■ \bold{produceAlias}\cr■ \bold{produceArguments}\cr■ \bold{produceAuthor}\cr■ \bold{produceConcept}\cr■ \bold{produceCustom_section}\cr■ \bold{produceDescription}\cr■ \bold{produceDetails}\cr■ \bold{produceDocType}\cr■ \bold{produceEncoding}\cr■ \bold{produceExamples}\cr■ \bold{produceFormat}\cr■ \bold{produceKeyword}\cr■ \bold{produceName}\cr■ \bold{produceNote}\cr■ \bold{produceRdOpts}\cr■ \bold{produceRdversion}\cr■ \bold{produceReferences}\cr■ \bold{produceSeealso}\cr■ \bold{produceSexpr}\cr■ \bold{produceSource}\cr■ \bold{produceSynopsis}\cr■ \bold{produceTitle}\cr■ \bold{produceUsage}\cr■ \bold{produceValue}\cr■ \bold{retrieveStrategy}\cr■ \bold{setUseMarkers}
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
ic <- InputContext(object_o_1, methodName_s_1 = NA_character_, packageName_s_1 = NA_character_, dataFilename_s_1 = NA_character_)
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
