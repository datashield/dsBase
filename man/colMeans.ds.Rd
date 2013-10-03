\name{colMeans.ds}
\alias{colMeans.ds}
<<<<<<< HEAD
\title{Calculates statistical mean for a numeric 1D array with more than 4 entries}
=======
\title{Calculates a statistical mean for a numeric 1D array with more than 4 entries}
>>>>>>> 3f9154d22721bd3d50feda7e9845d60d3e8e8cc5
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
<<<<<<< HEAD
  Calculates statistical mean for a numeric 1D array with
=======
  Calculates a statistical mean for a numeric 1D array with
>>>>>>> 3f9154d22721bd3d50feda7e9845d60d3e8e8cc5
  more than 4 entries
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

<<<<<<< HEAD
library(opal)
=======
library(dsbaseclient)
>>>>>>> 3f9154d22721bd3d50feda7e9845d60d3e8e8cc5
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(colMeans.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

