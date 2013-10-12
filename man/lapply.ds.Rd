\name{lapply.ds}
\alias{lapply.ds}
\title{Applies a function to each of the given categories}
\usage{
  lapply.ds(xvect = NULL, yvect = NULL, fun = NULL)
}
\arguments{
  \item{xvect}{a numeric vector}

  \item{yvect}{a factor vector}

  \item{fun}{the function to apply, \code{mean.ds} or
  \code{var.ds}.}
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
datashield.aggregate(opals, quote(lapply.ds(D$PM_BMI_CATEGORICAL, D$GENDER, fun=mean.ds)))

# compute the variance for the BMI categories that correspond to the two GENDER levels
datashield.aggregate(opals, quote(lapply.ds(D$PM_BMI_CATEGORICAL, D$GENDER, fun=var.ds)))
 }
}
\author{
  Gaye, A.
}

