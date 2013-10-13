\name{append2df.ds}
\alias{append2df.ds}
\title{Appends a column(s) to a dataframe}
\usage{
  append2df.ds(dataset, column, colname)
}
\arguments{
  \item{dataset}{a dataframe}

  \item{column}{a list which contains the column/vector to
  append to the dataframe}

  \item{colname}{a character, the name of the appended
  column}
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
# center LAB_HDL for each study
for(i in 1:length(opals)){
  call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
  datashield.assign(opals[i], "lab_hdl.c", call.object)
}

# now append 'lab_hdl.c' to the initially assigned dataframe 'D'
# and replace the initial dataframe 'D' by the new dataframe
call.object <- call("append2df.ds", quote(lab_hdl.cL), quote(as.character("lab_hdl.c")))
datashield.assign(opals, "Dnew", call.object)
datashield.assign(opals, "D", quote(Dnew))
}
}
\author{
  Gaye, A.
}

