\name{table1d.ds}
\alias{table1d.ds}
\title{Creates 1-dimensional contingency tables - potentially disclosive data are suppressed}
\usage{
  table1d.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector with discrete values -
  usually a factor}
}
\value{
  A list object (eg. named out.obj) with one component from
  each separate study which contains the following items:
  \item{is.table.valid}{eg. out.obj[[2]]$is.table.valid is
  a logical (Boolean) indicator of whether the data in
  study 2 is entirely valid (TRUE) for the stated variable
  or at least one category is invalid (FALSE)}
  \item{safe.table}{eg. out.obj[[1]]$safe.table contains
  the safe (non-disclosive) one dimensional table that can
  be released from study 1 - unsafe data are concealed by
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
  user). If the count in any cell in the table produced by
  a given data source IS invalid, that cell count is
  changed to "-1" and the name of the category that
  corresponds to it is changed to "-9". Each source is
  flagged as having only valid data, or at least some
  invalid data. Missing data are treated as
  na.action=na.omit (default for table function), if
  missing data are to be treated as a separate and visible
  category, the variable should first be transformed to
  convert NAs to the a new value.
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar  <- list("DIS_CVA")
opals  <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate a one dimensional table
datashield.aggregate(opals, quote(table1d.ds(D$DIS_CVA)))

# Example 2: generate a one dimensional table, outputting study specific and combined contingency tables, see what happens if
you try to tabulate a quantitative variable with unique values for many individuals. The standard table() function in R would print out
all observed "values" as category names in ascending order with a count (generally 1) for each unique number, but ds.table1d prints
out all values where there are between 1 and 4 observations as -1 and gives the category name -9. It is only when the count is
5 or more that the actual value can be observed, and then it is non-disclosive
datashield.aggregate(opals, quote(table1d.ds(D$LAB_HDL)))
}
}
\author{
  Burton, P.
}

