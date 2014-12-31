\name{subclassDShelper1}
\alias{subclassDShelper1}
\title{generates subsets vectors from a factor vector}
\usage{
subclassDShelper1(xvect = NULL, xname = NULL, filter = NULL)
}
\arguments{
  \item{xvect}{a vector of type factor.}

  \item{xname}{the name of the vector.}

  \item{nfilter}{the minimum number observation (i.e. rows)
  that are allowed.}
}
\value{
a list which contains the subsets.
}
\description{
This is an internal function called by the function
'subclassDS'.
}
\details{


The function generates subsets if the input of 'subclassDS'
is a factor vector.
}
\author{
Gaye, A.
}
\keyword{internal}

