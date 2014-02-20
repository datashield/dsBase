\name{changerefgroup.ds}
\alias{changerefgroup.ds}
\title{changes a reference level of a factor}
\usage{
changerefgroup.ds(xvect, ref = NULL)
}
\arguments{
  \item{xvect}{a factor}

  \item{ref}{the reference level}
}
\value{
a factor of the same length as xvect
}
\description{
this function is similar to R function \code{relevel}, but
in addition addes numbering to the levels so that they are
displayed in the right order when creating cross-tables.
}
\details{

}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
myvar <- list("PM_BMI_CATEGORICAL")
library(opal)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# rename the levels of PM_BMI_CATEGORICAL and make "obesity" as a reference level
datashield.assign(opals, 'bmi_new', quote(recodelevels.ds(D$PM_BMI_CATEGORICAL, newlabels=c('normal', 'overweight', 'obesity'))))
datashield.assign(opals, 'bmi_ob', quote(changerefgroup.ds(bmi_new, ref='2_obesity')))
ds.levels(opals, quote(bmi_ob))

# or without renaming the levels (group "3" as a reference level)
datashield.assign(opals, 'bmi_ob', quote(changerefgroup.ds(D$PM_BMI_CATEGORICAL, ref='3')))
ds.levels(opals, quote(bmi_ob))
}
}
\author{
Isaeva, J.
}

