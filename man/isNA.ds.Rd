\name{isNA.ds}
\alias{isNA.ds}
\title{Checks if a vector is empty}
\usage{
  isNA.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical or character vector}
}
\value{
  the integer '1' if the vector contains on NAs and '0'
  otherwise
}
\description{
  this function is similar to R function \code{is.na} but
  instead of a vector of booleans it returns just one
  boolean to tell if all the element are missing values.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
library(dsbaseclient)
myvar <- list("LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# check if the variable 'LAB_HDL' is empty
datashield.aggregate(opals, quote(isNA.ds(D$LAB_HDL)))
}
}
\author{
  Gaye, A.
}

