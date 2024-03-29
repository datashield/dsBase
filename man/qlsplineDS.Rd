% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qlsplineDS.R
\name{qlsplineDS}
\alias{qlsplineDS}
\title{Basis for a piecewise linear spline with meaningful coefficients}
\usage{
qlsplineDS(x = x, q = q, na.rm = TRUE, marginal = FALSE, names = NULL)
}
\arguments{
\item{x}{the name of the input numeric variable}

\item{q}{numeric, a single scalar greater or equal to 2 for a number of equal-frequency
intervals along x or a vector of numbers in (0; 1) specifying the quantiles explicitely.}

\item{na.rm}{logical, whether NA should be removed when calculating quantiles, passed
to na.rm of quantile. Default set to TRUE.}

\item{marginal}{logical, how to parametrize the spline, see Details}

\item{names}{character, vector of names for constructed variables}
}
\value{
an object of class "lspline" and "matrix", which its name is specified by the
\code{newobj} argument (or its default name "qlspline.newobj"), is assigned on the serverside.
}
\description{
This function is based on the native R function \code{qlspline} from the
\code{lspline} package. This function computes the basis of piecewise-linear spline
such that, depending on the argument marginal, the coefficients can be interpreted as
(1) slopes of consecutive spline segments, or (2) slope change at consecutive knots.
}
\details{
If marginal is FALSE (default) the coefficients of the spline correspond to
slopes of the consecutive segments. If it is TRUE the first coefficient correspond to
the slope of the first segment. The consecutive coefficients correspond to the change
in slope as compared to the previous segment.
Function qlspline wraps lspline and calculates the knot positions to be at quantiles
of x. If q is a numerical scalar greater or equal to 2, the quantiles are computed at 
seq(0, 1, length.out = q + 1)[-c(1, q+1)], i.e. knots are at q-tiles of the distribution
of x. Alternatively, q can be a vector of values in [0; 1] specifying the quantile
probabilities directly (the vector is passed to argument probs of quantile).
}
\author{
Demetris Avraam for DataSHIELD Development Team
}
