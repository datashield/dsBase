\name{c.ds}
\alias{c.ds}
\title{Combine Values into a Vector or List}
\usage{
  c.ds(vector)
}
\arguments{
  \item{vector}{a list of string characters, the names of
  the objects to combine}
}
\value{
  a vector
}
\description{
  Combine Values into a Vector or List
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
library(opal)
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# combine LAB_TSC and LAB_HDL
mylist <- list("D$LAB_TSC", "D$LAB_HDL")
datashield.assign(opals, "cvect", quote(c(mylist)))
}
}
\author{
  Gaye, A.
}

