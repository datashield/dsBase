\name{isValidDS}
\alias{isValidDS}
\title{Checks if an input is valid}
\usage{
isValidDS(obj)
}
\arguments{
  \item{obj,}{a vector (numeric, integer, factor,
  character), data.frame or matrix}
}
\value{
a boolean TRUE if input is valid or stops the process
otherwise
}
\description{
Internal function
}
\details{


This function checks if an input variable is valid. A
numeric variable is not valid if it has > 0 and < 5
observations and a factor variable is not valid if any of
its levels (classes) has a count of between 1 and 4.
}
\author{
Gaye, A.
}
\keyword{internal}

