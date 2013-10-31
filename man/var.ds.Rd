\name{var.ds}
\alias{var.ds}
\title{Computes the variance of a numeric vector}
\usage{
  var.ds(xvect)
}
\arguments{
  \item{xvect}{a numeric vector}
}
\value{
  a numeric, the variance
}
\description{
  Computes the variance of a numeric vector
}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the variance
stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

