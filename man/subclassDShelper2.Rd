\name{subclassDShelper2}
\alias{subclassDShelper2}
\title{generates subset tables from a data frame}
\usage{
subclassDShelper2(df = NULL, iter = NULL, filter = NULL)
}
\arguments{
  \item{df}{a data frame.}

  \item{iter}{the indices of columns to loop trough.}

  \item{filter}{the minimum number of observations (i.e.
  rows) that are allowed.}
}
\value{
a list which contains the subsets, their names and an
integer that indicates how many columns were not factors.
}
\description{
This is an internal function called by the function
'subclassDS'
}
\details{


The function generates subsets if the input of 'subclassDS'
is a data frame and if the number variables(columns) to
subset by are greater than 1; i.e. this function is called
if the user specified more than one variable or no variable
to subset by (if no variables are specified the function
'subclassDS' produces a subset for each category in each
variable).
}
\author{
Gaye, A.
}
\keyword{internal}

