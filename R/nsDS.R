#' 
#' @title Generate a Basis Matrix for Natural Cubic Splines
#' @description This function is based on the native R function \code{ns} from the
#' \code{splines} package. This function generate the B-spline basis matrix for a natural
#' cubic spline.
#' @details \code{ns} is native R is based on the function \code{splineDesign}. It generates
#' a basis matrix for representing the family of piecewise-cubic splines with the specified
#' sequence of interior knots, and the natural boundary conditions. These enforce the constraint
#' that the function is linear beyond the boundary knots, which can either be supplied or default
#' to the extremes of the data.
#' A primary use is in modeling formula to directly specify a natural spline term in a model.
#' @param x the predictor variable. Missing values are allowed.
#' @param df degrees of freedom. One can supply df rather than knots; ns() then chooses 
#' df - 1 - intercept knots at suitably chosen quantiles of x (which will ignore missing values).
#' The default, df = NULL, sets the number of inner knots as length(knots).
#' @param knots breakpoints that define the spline. The default is no knots; together with the
#' natural boundary conditions this results in a basis for linear regression on x. Typical values
#' are the mean or median for one knot, quantiles for more knots. See also Boundary.knots.
#' @param intercept if TRUE, an intercept is included in the basis; default is FALSE.
#' @param Boundary.knots boundary points at which to impose the natural boundary conditions and
#' anchor the B-spline basis (default the range of the data). If both knots and Boundary.knots
#' are supplied, the basis parameters do not depend on x. Data can extend beyond Boundary.knots
#' @return A matrix of dimension length(x) * df where either df was supplied or if knots were
#' supplied, df = length(knots) + 1 + intercept. Attributes are returned that correspond to the
#' arguments to ns, and explicitly give the knots, Boundary.knots etc for use by predict.ns().
#' The object is assigned at each serverside.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
nsDS <- function(x, df, knots, intercept, Boundary.knots){
  
  # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab) 
  
  x <- eval(parse(text=x), envir = parent.frame())
  
  if(is.null(Boundary.knots)){
    Boundary.knots <- range(x, na.rm=TRUE)
  }
  
  out <- splines::ns(x = x, df = df, knots = knots, intercept = intercept, Boundary.knots = Boundary.knots)
  
  for(i in 1:ncol(out)){
    if(length(unique(out[,i])) <= nfilter.tab){
      stop(paste0("One of the spline segments has less than ", nfilter.tab, 
                  " observations. Please redefine the knot positions"), call.=FALSE)
    }
  }
  
  return(out)
  
}
# ASSIGN FUNCTION
# nsDS
