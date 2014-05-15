\name{covDS}
\alias{covDS}
\title{Computes covariance between two or more vectors}
\usage{
covDS(x = NULL, y = NULL, use = NULL)
}
\arguments{
  \item{x}{a character, the name of a vector, dataframe or
  matrix}

  \item{y}{(optional) a character, the name of a vector,
  dataframe or matrix}

  \item{use}{a character string giving a method for
  computing covariances in the presence of missing values.
  This must be one of the strings: "everything", "all.obs",
  "complete.obs", "na.or.complete", or
  "pairwise.complete.obs".}
}
\value{
covariance
}
\description{
this function is similar to R function 'cov'
}
\details{

}
\author{
Gaye, A.
}

