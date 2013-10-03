\name{histogram.ds}
\alias{histogram.ds}
\title{Computes a histogram of the given data values without plotting.}
\usage{
  histogram.ds(xvect, brks)
}
\arguments{
  \item{xvect}{the numeric vector for which the histogram
  is desired.}

  \item{brks}{a numerical vector giving the breakpoints
  between histogram cells}
}
\value{
  a list with an object of class \code{histogram} and a
  vector of x-positions
}
\description{
  this functions produces the information required to plot
  a histogram. This is done without allowing for bins
  (cells) with a count of less than 5. If a bin has a count
  < 5 it is collapsed with the nearing bin; this process
  iterates until all bins have count >=5.
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

library(opal)
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a histogram object without plotting
hist.object <- datashield.aggregate(opals, quote(histogram.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

