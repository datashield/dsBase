\name{subsetdata.ds}
\alias{subsetdata.ds}
\title{Generates a valid subset of a dataset}
\usage{
  subsetdata.ds(dataset = NULL, columns = NULL)
}
\arguments{
  \item{dataset}{a string character, the name of the
  dataset.}

  \item{columns}{a a numeric vector that gives the subsets
  of the variables for which subsets are sougth.}
}
\value{
  a list which contains the subsetted datasets
}
\description{
  Generates a valid subset of a dataset
}
\details{
  The function takes a dataframe and generates a subset
  dataframes for all the factor variables if that subset is
  valid. By default a subset of the dataframe is created
  for each categorie of each factor. It is possible to
  indicate the variable for which a subset is sought by
  indicating their columns indices. The variable at the
  given indices are not factors no subsets are generated.
}
\examples{
\dontrun{
# load the login data
library(opal)
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: get the subsets from the table assigned above (by default is 'D')
datashield.assign(opals, "Subsets", quote(subsetdata.ds("D")))

#' # Example 2: get the subsets from the table assigned above (by default is 'D')
datashield.assign(opals, "Subsets", quote(subsetdata.ds("D", list(4))))
}
}
\author{
  Gaye, A.
}

