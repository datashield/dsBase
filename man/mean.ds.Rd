\name{mean.ds}
\alias{mean.ds}
\title{Computes statistical mean of vector with more than 4 entries}
\usage{
\method{mean}{ds}(xvect)
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

}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
}
}
\author{
Gaye, A.
}

