\name{subsetdata.ds}
\alias{subsetdata.ds}
\title{Generates a valid subset of a dataset}
\usage{
  subsetdata.ds(dataset = NULL)
}
\arguments{
  \item{dataset}{a dataframe}
}
\value{
  a list which contains the subsetted datasets
}
\description{
  Generates a valid subset of a dataset
}
\details{
  the function takes a dataframe and generates a subsets
  dataframes for all the factor variables if that subset is
  valid. with new levels. This is done for each study.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get the subsets from the table assigned above (by default is 'D')
datashield.assign(opals, "Subsets", quote(subsetdata.ds(D)))
}
}
\author{
  Gaye, A.
}

