\name{InputContext-class}
\docType{class}
\alias{InputContext-class}
\alias{InputContext}
\title{Input Context}
\description{
Environment class InputContext. Defines and eases input context management.
}
\usage{
InputContext(object_o_1,
             methodName_s_1 = NA_character_,
             packageName_s_1 = NA_character_,
             dataFilename_s_1 = NA_character_
)
}
\arguments{
\item{object_o_1}{The object to consider, or \code{NULL}}
\item{methodName_s_1}{A single string value that is the method name to consider}
\item{packageName_s_1}{The target package name to use}
\item{dataFilename_s_1}{A single string that is the data file name}
}
\value{
An object instance of class \code{InputContext}.
}
\section{Information}{
\subsection{Environment fields}{
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{beautifier} list\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{class_kind} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{class_name} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{data_name} NULL\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{dataFilename_s_1} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{file_name} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{hack_description} logical\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{instrumentationLevel} list\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{kind} double\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{kinds} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{methodName_s_1} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{number_replacements} integer\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{object_o_1} list\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{packageName_s_1} character\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{self} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{typeFactory_o_1} environment\cr
\ifelse{latex}{\out{$\\mapsto$}}{\ifelse{html}{\out{&#x25C6;}}{\\u25c6}} \bold{use_markers} logical
}
\subsection{Environment methods}{
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{buildMethodName}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{generateConditionalMarker}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{generateConditionalMarkerGenerator}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{getFilename}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{getKind}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{getName}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{markerGenerator}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceAlias}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceArguments}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceAuthor}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceConcept}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceCustom_section}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceDescription}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceDetails}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceDocType}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceEncoding}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceExamples}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceFormat}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceKeyword}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceName}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceNote}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceRdOpts}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceRdversion}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceReferences}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceSeealso}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceSexpr}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceSource}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceSynopsis}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceTitle}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceUsage}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{produceValue}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{retrieveStrategy}\cr
\ifelse{latex}{\out{$\\rightarrow$}}{\ifelse{html}{\out{&#x25A0;}}{\\u25a0}} \bold{setUseMarkers}
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
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\seealso{
Class \code{\link{ProcessingContext}} class \code{\link{GenerationContext}}
and class \code{\link{ManualPageBuilder}}.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CORE

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} BUILD

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} CONTENT_GENERATION
}
\examples{
ic1 <- InputContext(NULL)

ic2 <- InputContext(NULL, 'append', package = 'my.package.name')
}
