\name{means.ds}
\alias{means.ds}
\title{Calculates statistical mean of vector with more than 4 entries}
\usage{
  means.ds(xvect)
}
\arguments{
  \item{xvect}{a vector}
}
\value{
  a numeric, the statistical mean
}
\description{
  Calculates statistical mean of vector with more than 4
  entries
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

library(opal)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

