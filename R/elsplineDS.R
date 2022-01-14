#' 
#' @title Basis for a piecewise linear spline with meaningful coefficients
#' @description This function is based on the native R function \code{elspline} from the
#' \code{lspline} package. This function computes the basis of piecewise-linear spline
#' such that, depending on the argument marginal, the coefficients can be interpreted as
#' (1) slopes of consecutive spline segments, or (2) slope change at consecutive knots.
#' @details If marginal is FALSE (default) the coefficients of the spline correspond to
#' slopes of the consecutive segments. If it is TRUE the first coefficient correspond to
#' the slope of the first segment. The consecutive coefficients correspond to the change
#' in slope as compared to the previous segment.
#' Function elspline wraps lspline and computes the knot positions such that they cut the
#' range of x into n equal-width intervals.
#' @param x the name of the input numeric variable
#' @param n integer greater than 2, knots are computed such that they cut n equally-spaced
#' intervals along the range of x
#' @param marginal logical, how to parametrize the spline, see Details
#' @param names character, vector of names for constructed variables
#' @return an object of class "lspline" and "matrix", which its name is specified by the
#' \code{newobj} argument (or its default name "elspline.newobj"), is assigned on the serverside.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
elsplineDS <- function(x = x, n = n, marginal = FALSE, names = NULL){
  
  # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab) 
  
  # this is a copy of the lspline function from lspline R package (version 1.0-0)
  # we use this copy just to avoid having one more package as a dependency of dsBase
  lspline_copy <- function (x, knots = NULL, marginal = FALSE, names = NULL) 
  {
    if (!is.null(names)) {
      .NotYetUsed("names")
    }
    n <- length(x)
    nvars <- length(knots) + 1
    namex <- deparse(substitute(x))
    knots <- sort(knots)
    if (marginal) {
      rval <- cbind(x, sapply(knots, function(k) ifelse((x - k) > 0, x - k, 0)))
    }
    else {
      rval <- matrix(0, nrow = n, ncol = nvars)
      rval[, 1] <- pmin(x, knots[1])
      rval[, nvars] <- pmax(x, knots[length(knots)]) - knots[length(knots)]
      if (nvars > 2) {
        for (i in seq(2, nvars - 1)) {
          rval[, i] <- pmax(pmin(x, knots[i]), knots[i - 1]) - knots[i - 1]
        }
      }
    }
    colnames(rval) <- seq(1, ncol(rval))
    structure(rval, knots = knots, marginal = marginal, class = c("lspline", "matrix"))
  }
  
  x <- eval(parse(text=x), envir = parent.frame())
  
  stopifnot(n >= 2)
  k <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)[-c(1, n + 1)]

  out <- lspline_copy(x = x, knots = k, marginal = marginal, names = names)
  
  for(i in 1:ncol(out)){
    if(length(unique(out[,i])) <= nfilter.tab){
      stop(paste0("One of the spline segments has less than ", nfilter.tab, 
                  " observations. Please redefine the value of n"), call.=FALSE)
    }
  }
  
  return(out)
  
}
# ASSIGN FUNCTION
# elsplineDS
