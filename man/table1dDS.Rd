\name{table1dDS}
\alias{table1dDS}
\title{Creates 1-dimensional contingency tables - potentially disclosive data are suppressed}
\usage{
table1dDS(xvect)
}
\arguments{
  \item{xvect}{a numerical vector with discrete values -
  usually a factor}
}
\value{
A list object (eg. named out.obj) with one component from
each separate study which contains the following items:
\item{is.table.valid}{eg. out.obj[[2]]$is.table.valid is a
logical (Boolean) indicator of whether the data in study 2
is entirely valid (TRUE) for the stated variable or at
least one category is invalid (FALSE)}
\item{safe.table}{eg. out.obj[[1]]$safe.table contains the
safe (non-disclosive) one dimensional table that can be
released from study 1 - unsafe data are concealed by
converting invalid counts to "-1" and their corresponding
catregory identifier to "-9"}
}
\description{
The function table1d.ds is a server-side subfunction of
datashield.aggregate(). It generates 1-dimensional tables
for all data sources. Valid (non-disclosive) data are
defined as data from sources where no table cells have
counts between 1 and 4 (the upper value [4] can in
principle be changed but only by rewriting the underlying
function - it cannot be changed by a standard DataSHIELD
user). If the count in any cell in the table produced by a
given data source IS invalid, that cell count is changed to
"-1" and the name of the category that corresponds to it is
changed to "-9". Each source is flagged as having only
valid data, or at least some invalid data. Missing data are
treated as na.action=na.omit (default for table function),
if missing data are to be treated as a separate and visible
category, the variable should first be transformed to
convert NAs to the a new value.
}
\details{

}
\author{
Burton, P.
}

