\name{cut}
\alias{cut}
\title{Converts a continuous variable into a categorical one}
\usage{
  cut(xvect, breaks = quantile(xvect, na.rm = T),
    labels = F)
}
\arguments{
  \item{xvect}{a continuous numeric vector}

  \item{breaks}{a numeric vector of two or more unique cut
  points giving the numberof intervals into which ‘xvect’
  is to be cut. By default, the quantile function is used
  to set the breaks to obtain categories of roughly equal
  content.}

  \item{labels}{labels for the resulting category. By
  default integers started from '1' are used to label the
  categories}
}
\description{
  Converts a continuous variable into a categorical one
}

