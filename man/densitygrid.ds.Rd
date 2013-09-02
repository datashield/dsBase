\name{densitygrid.ds}
\alias{densitygrid.ds}
\title{Generates a density grid with or without a priori defined limits}
\usage{
  densitygrid.ds(xvect, yvect, limits = FALSE,
    x.min = NULL, x.max = NULL, y.min = NULL, y.max = NULL,
    numints = 20)
}
\arguments{
  \item{xvect}{a numerical vector}

  \item{yvect}{a numerical vector}

  \item{limits}{a logical expression for whether or not
  limits of the density grid are defined by a user If
  \code{limits} is set to "FALSE", min and max of xvect and
  yvect are used as a raneg If \code{limits} is st to
  "TRUE", limits defined by x.min, x.max,y.min and y.max
  are used}

  \item{x.min}{a minimum value for the x axis of the grid
  density object, if needed}

  \item{x.max}{a maximum value for the x axis of the grid
  density object, if needed}

  \item{y.min}{a minimum value for the y axis of the grid
  density object, if needed}

  \item{y.max}{a maximum value for the y axis of the grid
  density object, if needed}

  \item{numints}{a number of intervals for the grid density
  object, by default is 20}
}
\value{
  a grid density matrix
}
\description{
  Generates a density grid with or without a priori defined
  limits
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
library(dsbaseclient)
myvar <- list("LAB_TSC","LAB_HDL")
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate a density grid matrix
datashield.aggregate(opals, quote(densitygrid.ds(D$LAB_TSC, D$LAB_HDL)))

# Example 2: generate a density grid matrix; SHOULD NOT WORK as \\code{limits=F}!
datashield.aggregate(opals, quote(densitygrid.ds(D$LAB_TSC, D$LAB_HDL, x.min=3)))

# Example 3: generate a density grid matrix with defined limits
datashield.aggregate(opals, quote(densitygrid.ds(D$LAB_TSC, D$LAB_HDL, limits=T, x.min=1,x.max=11,y.min=0,y.max=3)))

# Example 4: generate a density grid matrix; SHOULD NOT WORK as no defined limits are provided.
datashield.aggregate(opals, quote(densitygrid.ds(D$LAB_TSC, D$LAB_HDL, limits=T)))
}
}
\author{
  Isaeva, J. and Gaye, A.
}

