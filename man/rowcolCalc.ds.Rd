\name{rowcolCalc.ds}
\alias{rowcolCalc.ds}
\title{Computes sums and means of rows or columns of numeric arrays}
\usage{
  rowcolCalc.ds(dataset, operation)
}
\arguments{
  \item{dataset}{an array of two or more dimensions.}

  \item{operation}{a string character that indicates the
  operation to carry out: 'rowSums', 'colSums', 'rowMeans'
  or 'colMeans'}
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
datashield.assign(opals, dataset=quote(D), operation=as.character("rowSums"))
}
}
\author{
  Gaye, A.
}

