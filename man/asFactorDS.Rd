\name{asFactorDS}
\alias{asFactorDS}
\title{Turns a numeric vector into a factor vector}
\usage{
asFactorDS(x = character(), levels, labels = levels, exclude = NA,
  ordered = is.ordered(x))
}
\arguments{
  \item{x}{a vector of data, usually taking a small number
  of distinct values.}

  \item{levels}{an optional vector of the values (as
  character strings) that x might have taken.}

  \item{labels}{either an optional character vector of
  labels for the levels (in the same order as levels after
  removing those in exclude), or a character string of
  length 1.}

  \item{exclude}{a vector of values to be excluded when
  forming the set of levels. This should be of the same
  type as x, and will be coerced if necessary.}

  \item{ordered}{logical flag to determine if the levels
  should be regarded as ordered (in the order given).}
}
\value{
an object of class 'factor' or NULL if the resulting factor
is invalid
}
\description{
this function is similar to R function \code{as.factor}
except for the fact that it does not allow for levels with
one observation only.
}
\details{

}
\author{
Burton, P.; Gaye, A.
}

