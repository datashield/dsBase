\name{varDS}
\alias{varDS}
\title{Computes the variance of vector with more than 4 entries}
\usage{
varDS(xvect)
}
\arguments{
  \item{xvect}{a vector}
}
\value{
a numeric, the variance
}
\description{
Calculates the variance.
}
\details{


if the length of input vector is less than the set filter a
missing value is returned.
}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the variance
variance <- datashield.aggregate(opals, quote(varDS(D$LAB_TSC)))
}
}
\author{
Gaye, A.
}

