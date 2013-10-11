\name{subsetvar.ds}
\alias{subsetvar.ds}
\title{Derives a valid binary variable from a continuous variable}
\usage{
  subsetvar.ds(xvect = NULL, operator = NULL,
    threshold = NULL)
}
\arguments{
  \item{xvect}{a continuous numeric variable}

  \item{operator}{an integer that sets the operator 1 for
  '>', 2 for '<', 3 for '>=', 4 for '<=', 5 for '==' and 6
  for '!='}

  \item{threshold}{a numeric value that sets the threshold}
}
\value{
  a vector where the entries that met the condition are
  assigned the value 1 or 0 otherwise
}
\description{
  Derives a valid binary variable from a continuous
  variable
}
\details{
  This function runs only if the set threshold (i.e. >=) is
  met or not met by 1 single observation.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("PM_BMI_CONTINUOUS")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# run the function with for the condition '>=70'
var <- quote(D$PM_BMI_CONTINUOUS)
operator <- 3
threshold <- 70
cally <-  call("subsetvar.ds", var, as.vector(operator), as.vector(70))
output <- datashield.assign(datasources=opals, cally)
}
}
\author{
  Gaye, A.
}

