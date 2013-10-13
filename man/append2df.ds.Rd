\name{append2df.ds}
\alias{append2df.ds}
\title{Appends a column(s) to a dataframe}
\usage{
  append2df.ds(dataset, columns)
}
\arguments{
  \item{dataset}{a dataframe}

  \item{columns}{a list of columns/vectors to append to the
  dataframe}
}
\value{
  a new dataframe is assigned to the datasources
}
\description{
  Appends a column(s) to a dataframe
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

library(opal)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar, symbol="D")

# generate a new variable (e.g. a mean centered LAB_HDL)
# get the mean of LAB_HDL
mean.lab_hdl <- datashield.aggregate(opals, quote(mean.ds(D$LAB_HDL)))
datashield.assign(opals, "lab_hdl.c", quote(D$LAB_HDL - mean.lab_hdl))

# now append 'lab_hdl.c' to the initially assigned dataframe 'D'
# and replace the initial dataframe 'D' by the new dataframe
datashield.assign(opals, "D", quote(append2df.ds(D,list(lab_hdl.c))))
}
}
\author{
  Gaye, A.
}

