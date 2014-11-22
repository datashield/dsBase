\name{replaceNaDS}
\alias{replaceNaDS}
\title{Replaces the missing values in a vector}
\usage{
replaceNaDS(xvect, replacements)
}
\arguments{
  \item{xvect}{a character, the name of the vector to
  process.}

  \item{replacements}{a list which contains the replacement
  value(s), a vector one or more values for each study. The
  length of the list must be equal to the number of servers
  the analyst is connected to.}
}
\value{
a new vector or a new table structure with the new vector
}
\description{
This function identifies missing values and replaces them
by a value or values specified by the analyst.
}
\details{


This function is used when the analyst prefer or requires
complete vectors. It is then possible the specify one value
for each missing value by first returning the number of
missing values using the function \code{numNaDS} but in
most cases it might be more sensible to replace all missing
values by one specific value e.g. replace all missing
values in a vector by the mean or median value. Once the
missing values have been replaced a new vector is created.
If the vector is within a table structure such as a data
frame a new data frame is generated with the new vector.
}
\author{
Gaye, A.
}

