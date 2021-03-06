% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{writeFileWithHeader}
\alias{writeFileWithHeader}
\title{Write Table to File With Header}
\usage{
writeFileWithHeader(
  x,
  file,
  sHeaderFile,
  nRowHeader = 3,
  sep = "\\t",
  verbose = FALSE
)
}
\arguments{
\item{x}{object to be written. Usually a data frame or matrix.}

\item{file}{either a character string naming a file or a connection open for
writing. "" indicates output to the console. See \link[utils]{write.table}.}

\item{sHeaderFile}{string. Filename of a file to copy the header from}

\item{nRowHeader}{integer. Default is 3. Number of rows in the header in the
header file. This many rows will be copied from the header file and
inserted at the top of the new file.}

\item{sep}{string. Default is "\\t" Values on each line of the file are
separated by this character. see \link[utils]{write.table}.}

\item{verbose}{boolean. Default is FALSE. TRUE prints more messages in
console.}
}
\description{
Writes a table to a text file using the header from an existing file. This
  makes it easy to re-write data files into the same format as they were
  received.
}
\seealso{
\link[utils]{write.table} \link[utils]{read.table}
}
