\name{MinMax.ds}
\alias{MinMax.ds}
\title{Generates a matrix with a minimum and maximum value, for two numerical vector}
\usage{
  MinMax.ds(xvect, yvect)
}
\arguments{
  \item{xvect}{a numerical vector}

  \item{yvect}{a numerical vector}
}
\value{
  a matrix
}
\description{
  This function used by another function to plot a heatmap
  graph.
}
\details{
  the minumum and maximum values are potentially disclosive
  particularly in the presence of 'outliers'. However
  although this function generates the two extreme values
  of a vector, it is not not disclosive because the minimum
  and maximum that are return are, each, multiplied by a
  very small random value from a uniform distribution. That
  way the 'exact' minimum and maximum are not returned but
  at the same them the limits are not sensibly altered.
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
library(dsbaseclient)
myvar <- list("LAB_TSC")
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)

# generate the 'blured' minimum and maximum of the variable 'LAB_TSC'
datashield.aggregate(opals, quote(densitygrid.ds(D$LAB_TSC)))
}
}
\author{
  Isaeva, J. and Gaye A.
}

