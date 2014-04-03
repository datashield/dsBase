\name{meanDS}
\alias{meanDS}
\title{Computes statistical mean of vector with more than 4 entries}
\usage{
meanDS(xvect)
}
\arguments{
  \item{xvect}{a vector}
}
\value{
a numeric, the statistical mean
}
\description{
Calculates the mean value.
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

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(meanDS(D$LAB_TSC)))
}
}
\author{
Gaye, A.
}

