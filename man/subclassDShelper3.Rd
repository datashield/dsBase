\name{subclassDShelper3}
\alias{subclassDShelper3}
\title{generates subset tables from a data frame}
\usage{
subclassDShelper3(df = NULL, indx1 = NULL, filter = NULL)
}
\arguments{
  \item{df}{a data frame.}

  \item{indx1}{the column index of the variable specified
  by the user.}

  \item{filter}{the minimum number of observations (i.e.
  rows) that are allowed.}
}
\value{
a list which contains the subsets, their names and an
integer that indicates if the variable specified by user is
a factor.
}
\description{
This is an internal function called by the function
'subclassDS'
}
\details{


The function generates subsets if the input of 'subclassDS'
is a data frame and if the number of variables (columns) to
subset by is 1; i.e. this function is called if the user
specified one variable to subset by.
}
\author{
Gaye, A.
}
\keyword{internal}

