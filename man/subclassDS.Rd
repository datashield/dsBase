\name{subclassDS}
\alias{subclassDS}
\title{Generates valid subset(s) of a dataframe or a factor}
\usage{
subclassDS(data = NULL, variables = NULL)
}
\arguments{
  \item{data}{a string character, the name of the dataframe
  or the factor vector}

  \item{variables}{a vector of string characters, the names
  of the the variables to subset on.}
}
\value{
a list which contains the subsetted datasets
}
\description{
The function takes a categorical vector or dataframe as
input and generates subset(s) vectors or dataframes for
each category. Subsets are considered invalid if they hold
between 1 and 4 observations.
}
\details{


If the input data object is a dataframe it is possible to
specify the variables to subset on. If a subset is not
'valid' all its the values are reported as missing (i.e.
NA), the name of the subsets is labelled as '_INVALID'. If
no variables are specified to subset on, the dataframe will
be subset on each of its factor variables. And if none of
the columns holds a factyopr variable a message is issued
as output. A message is also issued as output if the input
vector is not of type factor.
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
datashield.assign(opals, "Subsets", quote(subclassDS("D")))

#' # Example 2: get the subsets by gender class from the table assigned above
# by default the name of the the assigned dataframe is 'D')
datashield.assign(opals, "Subsets", quote(subclassDS("D", c("GENDER"))))

#' # Example 3: split the variable 'GENDER' into two vectors: males and females
datashield.assign(opals, "gender", quote(D$GENDER))
datashield.assign(opals, "mf.tables", quote(subclassDS("gender")))
}
}
\author{
Gaye, A.
}

