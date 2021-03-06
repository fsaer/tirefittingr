% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PreProcess.R
\name{setFYPure2002}
\alias{setFYPure2002}
\alias{setFYMF52}
\title{Changes the Options to Run \code{FYPure2002}}
\usage{
setFYPure2002()

setFYMF52()
}
\description{
Sets the following options:\preformatted{  options(tirefittingr.sdfStartPop = "dfStartParFY",
    tirefittingr.sfFittingFunction = "FYPurePacejka2002",
    tirefittingr.sfPreProcess = "FYPre",
    tirefittingr.sfPlot = "fFYPlot")
}

So that \code{fitTires()} can be run for \code{FYPure2002} For all available
options, see the help file for the \code{\link{fitTires}} function.
Also see \code{\link{FYPurePacejka2002}}
}
\examples{
setFYPure2002()
}
