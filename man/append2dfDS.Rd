\name{append2dfDS}
\alias{append2dfDS}
\title{Appends a column to a dataframe}
\usage{
append2dfDS(dataset, xvect)
}
\arguments{
  \item{dataset}{a dataframe}

  \item{xvect}{the vector to append to the dataframe}
}
\value{
a new dataframe
}
\description{
append a column to a dataframe and creates a new dataframe.
}
\details{

}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a new variable (e.g. a mean centered LAB_HDL)
# get the mean of LAB_HDL
mean.lab_hdl <- datashield.aggregate(opals, quote(mean.ds(D$LAB_HDL)))
# center LAB_HDL for each study
for(i in 1:length(opals)){
  call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
  datashield.assign(opals[i], "lab_hdl.c", call.object)
}

# now append 'lab_hdl.c' to the initially assigned dataframe 'D'
# without replacing the initial dataframe 'D'
call.object <- call("append2dfDS", quote(D), quote(lab_hdl.c))
datashield.assign(opals, call.object)
}
}
\author{
Gaye, A.
}

