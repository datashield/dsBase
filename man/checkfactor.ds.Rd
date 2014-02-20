\name{checkfactor.ds}
\alias{checkfactor.ds}
\title{Verifies that a factor vector is valid}
\usage{
checkfactor.ds(xvect)
}
\arguments{
  \item{xvect}{a numeric of character vector of type
  \code{factor}}
}
\value{
a list which contains two elements: \code{summary table},
the results of the tabulation of the input vector
\code{status}, tells if the factor is valid, 1 if one or
more levels have counts of between 0 and 5 and 0 otherwise.
}
\description{
This function checks if a numeric or character factor
vector contains levels with counts > 0 and < 5.
}
\details{

}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# check that the factor does not contain categories with count > 0 and < 5
output <- datashield.aggregate(opals, quote(checkfactor.ds(D$GENDER)))
}
}
\author{
Gaye, A.; Newby, C.
}

