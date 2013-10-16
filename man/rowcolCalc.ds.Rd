\name{rowcolCalc.ds}
\alias{rowcolCalc.ds}
\title{Computes sums and means of rows or columns of numeric arrays}
\usage{
  rowcolCalc.ds(dataset, operation)
}
\arguments{
  \item{dataset}{an array of two or more dimensions.}

  \item{operation}{an integer that indicates the operation
  to carry out: 1 for 'rowSums', 2 for 'colSums', 3 for
  'rowMeans' or 4 for 'colMeans'}
}
\value{
  a list that contains the vector of results.
}
\description{
  Computes sums and means of rows or columns of numeric
  arrays
}
\details{
  The function is similar to R base functions 'rowSums',
  'colSums', 'rowMeans' and 'colMeans' but its outcome
  cannot be returned to the user as it might be revealing
  if the array contains only one row or one column.
}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a histogram object without plotting
call.object <-  call("rowcolCalc.ds", quote(D), as.numeric(1))
datashield.assign(opals, "rowsums", call.object)
}
}
\author{
  Gaye, A.
}

