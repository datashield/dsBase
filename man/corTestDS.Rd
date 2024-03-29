% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corTestDS.R
\name{corTestDS}
\alias{corTestDS}
\title{Tests for correlation between paired samples}
\usage{
corTestDS(x, y, method, exact, conf.level)
}
\arguments{
\item{x}{a character string providing  the name of a numerical vector.}

\item{y}{a character string providing  the name of a numerical vector.}

\item{method}{a character string indicating which correlation coefficient is to be
used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.}

\item{exact}{a logical indicating whether an exact p-value should be computed. Used for
Kendall's tau and Spearman's rho.}

\item{conf.level}{confidence level for the returned confidence interval. Currently
only used for the Pearson product moment correlation coefficient if there are at least
4 complete pairs of observations.}
}
\value{
the results of the correlation test.
}
\description{
This function is similar to R function \code{cor.test}.
}
\details{
The function runs a two-sided correlation test
}
\author{
Demetris Avraam, for DataSHIELD Development Team
}
