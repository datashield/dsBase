\name{isValid.ds}
\alias{isValid.ds}
\title{Checks if an input is valid}
\usage{
  isValid.ds(xvect)
}
\arguments{
  \item{xvect}{a numeric or factor vector.}
}
\value{
  a boolean TRUE if input is valid or stops the process
  otherwise
}
\description{
  Checks if an input is valid
}
\details{
  This function checks if an input variable is valid. A
  numeric variable is not valid if it has > 0 and < 5
  observations and a factor variable is not valid if any of
  its levels (classes) has a count of between 1 and 4.
}
\examples{
\dontrun{
library(opal)
# load the file that contains the login details
data(logindata)

# login and assign a variable to R
opals <- datashield.login(logins=logindata,assign=TRUE,variables=list("LAB_HDL"))

# check if the variable LAB_HDL is valid
datashield.aggregate(opals, quote(isValid.ds(D$LAB_HDL)))
}
}
\author{
  Gaye, A.
}

