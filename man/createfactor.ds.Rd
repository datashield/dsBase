\name{createfactor.ds}
\alias{createfactor.ds}
\title{Generates a factor variable}
\usage{
  createfactor.ds(xvect = NULL, categories = NULL)
}
\arguments{
  \item{xvect}{a factor vector}

  \item{categories}{the levels of the factor to generate}
}
\value{
  a factor vector that has the levels of the original
  vector and some additional levels
}
\description{
  The functions uses the input factor and generates a new
  factor with new levels.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
library(opal)
myvar <- list("PM_BMI_CATEGORICAL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get the levels of variable PM_BMI_CATEGORICAL in study1
current.levels <- datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))

#' generate a new factor vector which has an additonal level '4'
newlevels <- c(current.levels[[1]], "4")
datashield.assign(opals, "bmi_f", quote(createfactor.ds(D$PM_BMI_CATEGORICAL, newlevels)))

 }
}
\author{
  Gaye, A.
}

