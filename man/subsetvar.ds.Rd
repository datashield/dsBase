\name{subsetvar.ds}
\alias{subsetvar.ds}
\title{Derives a valid binary variable from a continuous variable}
\usage{
  subsetvar.ds(xvect = NULL, operator = NULL,
    threshold = NULL)
}
\arguments{
  \item{xvect}{a continuous numeric variable}

  \item{operator}{a string character that represents the
  logical operator. Six logical operators are possible:
  '>', '<', '>=', '<=', '==' and '!='.}

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
  met or not met by more than one observation.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("PM_BMI_CONTINUOUS")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get a subset of PM_BMI_CONTINUOUS with the '>=70'
#' call.object <-  call("subsetvar.ds", quote(D$PM_BMI_CONTINUOUS), quote(as.character(">=")), quote(as.numeric(70)))
datashield.assign(opals, "subset", call.object)
}
}
\author{
  Gaye, A.
}

