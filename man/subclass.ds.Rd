\name{subclass.ds}
\alias{subclass.ds}
\title{Generates valid subset(s) of a dataset based on the subclasses.}
\usage{
subclass.ds(dataset = NULL, variables = NULL)
}
\arguments{
  \item{dataset}{a string character, the name of the
  dataset.}

  \item{variables}{a vector of string characters, the names
  of the variables for which subsets are sougth.}
}
\value{
a list which contains the subsetted datasets
}
\description{
The function takes a dataframe and generates subset(s)
dataframes for all (default) or specified factor variables
if the subsets are valid. By default a subset of the
dataframe is created for each categorie of each factor. It
is possible to indicate the variable for which a subset is
sought by indicating their name. If the variable indicated
are not factors no subsets are generated.
}
\details{

}
\examples{
\dontrun{
# load the login data
library(opal)
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: get the subsets for all categories from the table assigned above
# (by default the name of the the assigned dataframe is 'D')
datashield.assign(opals, "Subsets", quote(subclass.ds("D")))

#' # Example 2: get the subsets by gender class from the table assigned above
# by default the name of the the assigned dataframe is 'D')
datashield.assign(opals, "Subsets", quote(subclass.ds("D", c("GENDER"))))
}
}
\author{
Gaye, A.
}

