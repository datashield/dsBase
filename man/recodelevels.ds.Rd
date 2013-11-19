\name{recodelevels.ds}
\alias{recodelevels.ds}
\title{recodes a categorical variable}
\usage{
  recodelevels.ds(xvect, newlabels)
}
\arguments{
  \item{xvect}{a factor}

  \item{newlabels}{a string vector with new labels for the
  levels}
}
\value{
  a factor vector with new labels for levels
}
\description{
  this function recodes levels of a categorical variable
  with new given labels.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
myvar <- list("PM_BMI_CATEGORICAL")
library(opal)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get the range of the variable 'LAB_HDL'
datashield.assign(opals, 'bmi_new', quote(recodelevels.ds(D$PM_BMI_CATEGORICAL, newlabels=c('normal', 'overweight', 'obesity'))))
}
}
\author{
  Isaeva, J.
}

