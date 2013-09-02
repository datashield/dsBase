\name{createfactor.ds}
\alias{createfactor.ds}
\title{Generates a factor variable}
\usage{
  createfactor.ds(xvect, levels = NULL, labels = levels,
    exclude = NA, ordered = is.ordered(xvect))
}
\arguments{
  \item{xvect}{a numerical or character vector}

  \item{levels}{optional vector of values that 'xvect'
  might have taken}

  \item{labels}{labels for the different levels}

  \item{exclude}{values to be excluded}

  \item{ordered}{tells if the variable 'x' is ordered}
}
\value{
  a 'valid' factor vector or an empty vector of the same
  length.
}
\description{
  The functions uses the input vector to generate a factor
  vector. The generate factor is checked and if valid (no
  category with count > 0 and < 5) returned; if not valid
  an empty of the same length is returned.
}
\examples{
\dontrun{
# load the login data
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a factor vector using the variable 'GENDER'
datashield.assign(opals, "sex", quote(createfactor.ds(D$GENDER)))

# display the number of observation in each vector
datashield.aggregate(opals, quote(length(sex)))
 }
}
\author{
  Gaye, A.; Burton, P.
}

