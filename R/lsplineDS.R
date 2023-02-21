#' 
#' @title Basis for a piecewise linear spline with meaningful coefficients
#' @description This function is based on the native R function \code{lspline} from the
#' \code{lspline} package. This function computes the basis of piecewise-linear spline
#' such that, depending on the argument marginal, the coefficients can be interpreted as
#' (1) slopes of consecutive spline segments, or (2) slope change at consecutive knots.
#' @details If marginal is FALSE (default) the coefficients of the spline correspond to
#' slopes of the consecutive segments. If it is TRUE the first coefficient correspond to
#' the slope of the first segment. The consecutive coefficients correspond to the change
#' in slope as compared to the previous segment.
#' @param x the name of the input numeric variable
#' @param knots numeric vector of knot positions
#' @param marginal logical, how to parametrize the spline, see Details
#' @param names, character, vector of names for constructed variables
#' @return an object of class "lspline" and "matrix", which its name is specified by the
#' \code{newobj} argument (or its default name "lspline.newobj"), is assigned on the serverside.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
lsplineDS <- function(x = x, knots = NULL, marginal = FALSE, names = NULL){
  
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
  
  out <- lspline_copy(x = x, knots = knots, marginal = marginal, names = names)
  
  for(i in 1:ncol(out)){
    if(length(unique(out[,i])) <= nfilter.tab){
      stop(paste0("One of the spline segments has less than ", nfilter.tab, " observations. Please redefine the knot positions"), call.=FALSE)
    }
  }
  
  return(out)
  
}
# ASSIGN FUNCTION
# lsplineDS
