\name{.onAttach}
\alias{.onAttach}
\alias{addLoessLine}
\alias{addOutlierTestResults}
\alias{checkStartcatW}
\alias{ci.fp}
\alias{ci.fp1}
\alias{ciLabel}
\alias{getAllDependencies}
\alias{getFilePrefix}
\alias{getMainTitle}
\alias{getVarFromFormula}
\alias{hndlFormula}
\alias{hndlMultWhat}
\alias{legendHelp}
\alias{listSpecies}
\alias{makeColor}
\alias{makeFilename}
\alias{makeItemsToRemove}
\alias{pkolmogorov1x}
\alias{printProgressMsg}
\alias{processSessionInfo}
\alias{quad_dens}
\alias{typeoflm}
\title{Internal functions used in FSA.}
\usage{
.onAttach(lib, pkg, ...)
}
\description{
Internal functions used in FSA
}
\note{
Take note of the following uses: \itemize{ \item
\code{typeoflm} is used in \code{\link{fitPlot}} and
\code{\link{residPlot}}. \item \code{ciLabel} is used in
\code{\link{binCI}} and \code{\link{bootCase}}. \item
\code{hndlMultWhat} is used in \code{\link{ageBias}} and
\code{\link{agePrecision}}. \item \code{checkStartcatW}
used in \code{\link{lencat}} and
\code{\link{lenFreqExpand}}. \code{\link{catchCurve}},
\code{\link{chapmanRobson}}, \code{\link{confint.nlsBoot}},
\code{\link{depletion}}, \code{\link{hyperCI}},
\code{\link{mrClosed}}, \code{\link{poiCI}}, and
\code{\link{removal}}. \item \code{ci.fp} and \code{ci.fp1}
are used in \code{\link{fitPlot}}. \item
\code{addOutlierTestResults}, \code{addLoessLine}, and
\code{getMainTitle} are all used in
\code{\link{residPlot}}. \item \code{listSpecies} is used
in \code{\link{psdVal}}, \code{\link{psdCalc}},
\code{\link{psdPlot}}, \code{\link{wrAdd}}, and
\code{\link{wsVal}}. \item \code{getVarFromFormula} is used
in \code{\link{ageKey}}, \code{\link{lencat}}
\code{\link{psdCalc}}, \code{\link{psdDataPrep}},
\code{\link{psdPlot}}, and \code{\link{recodeSpecies}}.
\item \code{psdLitCheck} is used in \code{\link{psdVal}},
\code{\link{psdCalc}}, and \code{\link{psdPlot}}. \item
\code{wsLitCheck} is used in \code{\link{wsVal}} \item
\code{makeColor} is used in \code{\link{plotBinResp}} and
\code{\link{tictactoe}}. \item \code{getAllDependencies},
\code{getFilePrefix}, \code{makeFilename},
\code{makeItemsToRemove}, \code{printProgressMsg},
\code{processSessionInfo} are all used in the \code{swvXXX}
utility functions. \item \code{pkolmogorov1x} and
\code{quad_dens} are both used in \code{\link{ks2d1}} and
\code{\link{ks2d2}}. }
}
\keyword{internal}

