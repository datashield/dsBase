\name{histogramDS}
\alias{histogramDS}
\title{Computes a histogram of the given data values without plotting.}
\usage{
histogramDS(xvect, min, max, seed)
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
This functions produces the information required to plot a
histogram. This is done without allowing for bins (cells)
with a count of less than 5. If a bin has a count < 5 it is
collapsed with the nearing bin; this process iterates until
all bins have count >=5.
}
\details{

}
\author{
Gaye, A.
}

