\name{product.ds}
\alias{product.ds}
\title{Computes a product}
\usage{
  product.ds(xlist)
}
\arguments{
  \item{xlist}{a list of numerical to compute a product
  for.}
}
\value{
  a numerical
}
\description{
  Computes a product
}
\examples{
\dontrun{
# load the file that contains the login details
library(opal)
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
prodinput <- list(D$LAB_TSC, D$LAB_HDL)
cally <- call("product.ds", prodinput)
datashield.assign(opals, "P", cally)
}
}
\author{
  Gaye, A.
}

