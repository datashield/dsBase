% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lexisDS2.R
\name{lexisDS2}
\alias{lexisDS2}
\title{lexisDS2}
\usage{
lexisDS2(
  datatext = NULL,
  intervalWidth,
  maxmaxtime,
  idCol,
  entryCol,
  exitCol,
  statusCol,
  vartext = NULL
)
}
\arguments{
\item{datatext}{a clientside provided character string specifying the data.frame
holding the data set to be expanded}

\item{intervalWidth}{a clientside generated character string specifying the width
of the survival epochs in the
expanded data}

\item{maxmaxtime}{a clientside generated object specifying the maximum follow up
time in any of the sources}

\item{idCol}{a clientside generated character string specifying the variable
holding the IDs of individuals in the data set to be expanded}

\item{entryCol}{a clientside specified character string identifying the variable
holding the time that each individual starts follow up}

\item{exitCol}{a clientside specified character string identifying the variable
holding the time that each individual ends follow up (is censored or fails)}

\item{statusCol}{a clientside specified character string identifying the variable
holding the final censoring status (failed/censored)}

\item{vartext}{is a clientside provided vector of character strings denoting the
column names of additional variables to include in the 
final expanded table. If the 'variables' argument is not set (is null) but the
'data' argument is set the full data.frame will be expanded and carried forward}
}
\value{
List with `expanded.table`
}
\description{
The second serverside function called by ds.lexis.
}
\details{
This is the assign
function which actually creates
the expanded dataframe containing surival data for a piecewise exponential
regression. lexisDS2 also
carries out a series of disclosure checks and if the arguments or data fail any of
those tests,
creation of the expanded dataframe is blocked and an appropriate serverside error
message is stored.
For more details see the extensive header for ds.lexis.
}
\author{
Burton PR
}
