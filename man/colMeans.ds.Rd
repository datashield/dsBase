\name{colMeans.ds}
\alias{colMeans.ds}
\title{Calculates statistical mean for a numeric 1D array with more than 4 entries}
\usage{
  colMeans.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector}
}
\value{
  a numerical array, matrix or dataframe
}
\description{
  Calculates statistical mean for a numeric 1D array with
  more than 4 entries
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

library(opal)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(colMeans.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

