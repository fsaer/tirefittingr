% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PreProcess.R
\name{basicPre}
\alias{basicPre}
\title{Basic Preprocess for Tire Data}
\usage{
basicPre(dfData)
}
\arguments{
\item{dfData}{data frame. A dataset of tire data to be processed.}
}
\value{
data frame. The tire data trimmed down to the number of datapoints
  specified by `option("tirefittingr.iDataPoints")`.
}
\description{
Randomly reduces the number of data points to
  `getOption("tirefittingr.iDataPoints")` if a value is set.
  Defaults to 4000 otherwise. To set, run
  `options("tirefittingr.iDataPoints" = 4000")`, where 4000 is the number of
  data points you wish to use. For more info on options in tirefitingr, see
  the help file for the `fitTires` function with `?fitTires`.
  Works for both lateral and longitudinal.
}
\examples{
\dontrun{
PreData = basicPre(ABCrun1Lat) #Wnere ABCrun1Lat is a data frame
PreData = basicPre(readTTCData("C:/folder/Full_path_name.dat"))
}
}
