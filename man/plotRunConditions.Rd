% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Postprocess.R
\name{plotRunConditions}
\alias{plotRunConditions}
\title{Plots columns vs time of tire data}
\usage{
plotRunConditions(dfData, sRunName, sSavePath = NULL)
}
\arguments{
\item{dfData}{data frame. Processed tire data.}

\item{sRunName}{string. Name of the run.}

\item{sSavePath}{string. Defaults to NULL. Path to save the plot to. NULL
does not save the plot, and instead displays in the plot window.}
}
\value{
string. "Plot Run Conditions Successful". If no errors occurred.
}
\description{
Plots columns vs time of tire data
}
\examples{
\dontrun{
setFYPure2002() ## or setFXPure2002()
plotRunConditions(ABCrun1LongPreProccessed)
}
}
