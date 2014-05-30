\name{changeRefGroupDS}
\alias{changeRefGroupDS}
\title{Changes a reference level of a factor}
\usage{
changeRefGroupDS(xvect, ref = NULL, reorderByRef = NULL)
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


In addition to what the R function does, this function
allows for the user to re-order the vector, putting the
reference group first. If the user chooses the re-order a
warning is issued as this can introduce a mismatch of
values if the vector is put back into a table that is not
reordered in the same way. Such mismatch can render the
results of operations on that table invalid.
}
\author{
Isaeva, J., Gaye, A.
}

