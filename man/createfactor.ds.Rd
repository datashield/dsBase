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
a factor vector that has the levels of the original vector
and some additional levels
}
\description{
The functions uses the input factor and generates a new
factor with new levels.
}
\details{

}
\examples{
\dontrun{
# load the login data
library(opal)
data(logindata)

# login and assign specific variable(s)
myvar <- list("PM_BMI_CATEGORICAL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get the levels of variable PM_BMI_CATEGORICAL in study1
current.levels <- datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))

#' generate a new factor vector which has an additonal level '4'
call.object <- call("createfactor.ds", quote(D$PM_BMI_CATEGORICAL), as.list(current.levels[[1]], "4"))
datashield.assign(opals, "bmi_f", call.object)

 }
}
\author{
Gaye, A.
}

