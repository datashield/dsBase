% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsplineDS.R
\name{lsplineDS}
\alias{lsplineDS}
\title{Basis for a piecewise linear spline with meaningful coefficients}
\usage{
lsplineDS(x = x, knots = NULL, marginal = FALSE, names = NULL)
}
\arguments{
\item{x}{the name of the input numeric variable}

\item{knots}{numeric vector of knot positions}

\item{marginal}{logical, how to parametrize the spline, see Details}

\item{names, }{character, vector of names for constructed variables}
}
\value{
an object of class "lspline" and "matrix", which its name is specified by the
\code{newobj} argument (or its default name "lspline.newobj"), is assigned on the serverside.
}
\description{
This function is based on the native R function \code{lspline} from the
\code{lspline} package. This function computes the basis of piecewise-linear spline
such that, depending on the argument marginal, the coefficients can be interpreted as
(1) slopes of consecutive spline segments, or (2) slope change at consecutive knots.
}
\details{
If marginal is FALSE (default) the coefficients of the spline correspond to
slopes of the consecutive segments. If it is TRUE the first coefficient correspond to
the slope of the first segment. The consecutive coefficients correspond to the change
in slope as compared to the previous segment.
}
\author{
Demetris Avraam for DataSHIELD Development Team
}
