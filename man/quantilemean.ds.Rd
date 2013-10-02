\name{quantilemean.ds}
\alias{quantilemean.ds}
\title{Generates quantiles and mean information without maximum and minimum}
\usage{
  quantilemean.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector}
}
\value{
  a numeric vector that represents the sample quantiles
}
\description{
  the probabilities 5%, 10%, 25%, 50%, 75%, 90%, 95% and
  the mean are used to compute the corresponding quantiles
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

library(dsbaseclient)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the sample quantiles that correspond to the probabilities specified internally
quantiles <- datashield.aggregate(opals, quote(quantilemean.ds(D$LAB_TSC)))
}
}
\author{
  Burton, P.; Gaye, A.
}

