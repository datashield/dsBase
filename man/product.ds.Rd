\name{product.ds}
\alias{product.ds}
\title{Computes a product}
\usage{
  product.ds(a = 1, b = 1, c = 1, d = 1, e = 1)
}
\arguments{
  \item{a}{a numerical}

  \item{b}{a numerical}

  \item{c}{a numerical}

  \item{d}{a numerical}

  \item{e}{a numerical}
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
data(logindata)

# login and assign the required variables to R
library(dsbaseclient)
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
datashield.assign(opals, "P", quote(product.ds(D$LAB_TSC, D$LAB_HDL)))
}
}
\author{
  Burton, P.
}

