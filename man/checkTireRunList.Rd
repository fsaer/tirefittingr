% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PreProcess.R
\name{checkTireRunList}
\alias{checkTireRunList}
\title{Checks a list of tire runs for further processing}
\usage{
checkTireRunList(svInputFile = NULL, sDefaultPath = getwd())
}
\arguments{
\item{svInputFile}{string vector. A vector of complete file paths of
raw data files. Default value of NA causes a file selection dialog box
to open, where the user can select files dynamically.}

\item{sDefaultPath}{string. Default file path for the file dialog box.}
}
\value{
string vector. File paths of runs.
}
\description{
Checks a list of tire runs which can be run with the
  \code{\link{fitTires}} function.
}
\examples{
  \dontrun{
  lRunNames = checkTireRunList() #prompts user for files
  }
}
