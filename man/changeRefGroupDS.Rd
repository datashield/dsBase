\name{changeRefGroupDS}
\alias{changeRefGroupDS}
\title{Changes a reference level of a factor}
\usage{
changeRefGroupDS(xvect, ref = NULL)
}
\arguments{
  \item{xvect}{a factor}

  \item{ref}{the reference level}
}
\value{
a factor of the same length as xvect
}
\description{
This function is similar to R function \code{relevel}, but
in addition addes numbering to the levels so that they are
displayed in the right order when creating cross-tables.
}
\details{

}
\author{
Isaeva, J., Gaye, A.
}

