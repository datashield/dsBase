\name{createfactor.ds}
\alias{createfactor.ds}
\title{Generates a factor variable}
\usage{
  createfactor.ds(xvect = NULL, addLevels = NULL)
}
\arguments{
  \item{xvect}{a numerical or character vector}

  \item{addLevels}{the additional levels to add to the
  input factor}
}
\value{
  a factor vector that has its original levels plus the
  added levels
}
\description{
  The functions uses the input factor and generates a new
  factor with additional levels.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
library(opal)
myvar <- list("GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a factor vector using the variable 'GENDER'
datashield.assign(opals, "sex", quote(createfactor.ds(D$GENDER)))

# display the number of observation in each vector
datashield.aggregate(opals, quote(length(sex)))
 }
}
\author{
  Gaye, A.
}

