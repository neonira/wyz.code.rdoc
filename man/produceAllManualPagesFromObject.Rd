\name{produceAllManualPagesFromObject}
\alias{produceAllManualPagesFromObject}
\title{Produce All Manual Pages From Object}
\description{Produce object and methods manual pages from an object. }
\usage{
produceAllManualPagesFromObject(object_o_1,
                                processingContext_o_1 = ProcessingContext(),
                                generationContext_o_1 = GenerationContext(),
                                packageName_s_1 = NA_character_)
}
\arguments{
\item{object_o_1}{The single object you want to generate manual pages from.}
\item{processingContext_o_1}{The processing context object to consider for generation.
See \code{\link{ProcessingContext}}.}
\item{generationContext_o_1}{The generation context object to consider for generation.
See \code{\link{GenerationContext}}.}
\item{packageName_s_1}{The target package name to consider for generation. Allows to generate
automatically the \code{author} section content. }
}
\author{
\packageAuthor{wyz.code.rdoc}

Maintainer: \packageMaintainer{wyz.code.rdoc}
}
\note{
This is an \bold{EXPERIMENTAL function}. Prefer usage of function
\code{\link{produceManualPage}} instead.

It generates reliable individual manual pages
that taken all together are not fully compatible with \R way to express documentation.

In particular, expect duplicated aliases to appear, and some name weirdness also.
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\section{Classification}{
STRATUM \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} LAYER_3

PHASING \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} RUN

INTENT  \ifelse{latex}{\out{$\longmapsto$}}{\ifelse{html}{\out{&#x25B6;}}{\\u25b6}} QUALITY_CONTROL
}
