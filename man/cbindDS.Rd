\name{cbindDS}
\alias{cbindDS}
\title{Combines objects by columns}
\usage{
cbindDS(objs = NULL, clnames = NULL)
}
\arguments{
  \item{objs}{a list which contains the names of the
  objects to combine.}

  \item{clnames}{a list of charcaters, the column names of
  the output data frame.}
}
\value{
a dataframe
}
\description{
This function is similar to the R base function 'cbind'.
}
\details{


Unlike the R base function 'cbind' this function' output is
a dataframe as that is the type mainly used in DataSHIELD.
}
\author{
Gaye, A.
}

