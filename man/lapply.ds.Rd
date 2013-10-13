\name{lapply.ds}
\alias{lapply.ds}
\title{Applies a function to each of the given categories}
\usage{
  lapply.ds(xvect, yvect, fun)
}
\arguments{
  \item{xvect}{a numeric vector}

  \item{yvect}{a factor vector}

  \item{fun}{a character string, the name of the function
  to apply. The functions to apply are restricted to
  \code{mean.ds} and \code{var.ds}.}
}
\value{
  a list which contains the results of the applied function
  for each category.
}
\description{
  Applies a function to each of the given categories
}
\details{
  The function requires a numeric and a factor variable.
  The numeric values are categorised according to the
  factor levels. Two functions \code{mean.ds} and
  \code{var.ds} can then be applied each category.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
library(opal)
myvar <- list("PM_BMI_CATEGORICAL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the mean for the BMI categories that correspond to the two GENDER levels
call.object <- call("lapply.ds", quote(D$LAB_TSC), quote(D$GENDER), quote(as.character("mean.ds")))
datashield.aggregate(opals, call.object)

# compute the variance for the BMI categories that correspond to the two GENDER levels
call.object <- call("lapply.ds", quote(D$LAB_TSC), quote(D$GENDER), quote(as.character("var.ds")))
datashield.aggregate(opals, call.object)
 }
}
\author{
  Gaye, A.
}

