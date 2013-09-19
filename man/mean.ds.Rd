\name{mean.ds}
\alias{mean.ds}
\title{Calculates a statistical mean for a vector with more than 4 entries}
\usage{
  mean.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector}
}
\value{
  a numerical
}
\description{
  Calculates a statistical mean for a vector with more than
  4 entries
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

library(dsbaseclient)
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

