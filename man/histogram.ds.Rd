\name{histogram.ds}
\alias{histogram.ds}
\title{Computes a histogram of the given data values without plotting.}
\usage{
  histogram.ds(xvect, min, max, seed)
}
\arguments{
  \item{xvect}{the numeric vector for which the histogram
  is desired.}

  \item{min}{a numeric, the lower limit of the
  distribution.}

  \item{max}{a numeric, the upper limit of the
  distribution.}

  \item{seed}{an integer, the value to set the seed at when
  generating the break points.}
}
\value{
  a list with an object of class \code{histogram} and the
  number of invalid cells
}
\description{
  This functions produces the information required to plot
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

# get the range of the inout vector
ranges <- datashield.aggregate(datasources, quote(range.ds(xvect)))
minval <- ranges[[1]]
maxval <- ranges[[2]]
seedval <- 333
# generate a histogram object
hist.object <- datashield.aggregate(opals, quote(histogram.ds(xvect, minval, maxval, seedval)))
}
}
\author{
  Gaye, A.
}

