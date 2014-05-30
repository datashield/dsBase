\name{changeRefGroup.ds}
\alias{changeRefGroup.ds}
\title{Changes a reference level of a factor}
\usage{
changeRefGroup.ds(xvect, ref = NULL, reorderByRef = NULL)
}
\arguments{
  \item{xvect}{a factor vector}

  \item{ref}{a character, the reference level}

  \item{reorderByRef}{a boolean that tells whether or not
  the new vector should be ordered by the reference group.}
}
\value{
a factor of the same length as xvect
}
\description{
This function is similar to R function \code{relevel},
}
\details{


in addition to what the R function does, this function
allows for the user to re-order the vector putting the
reference group first.
}
\author{
Isaeva, J., Gaye, A.
}

