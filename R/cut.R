#' Converts a continuous variable into a categorical one
#'
#' @param xvect a continuous numeric vector
#' @param breaks a numeric vector of two or more unique cut points 
#' giving the numberof intervals into which ‘xvect’ is to be cut.
#' By default, the quantile function is used to set the breaks to
#' obtain categories of roughly equal content.
#' @param labels labels for the resulting category. By default integers
#' started from '1' are used to label the categories
#' @export
#' 

cut <- function (xvect, breaks=quantile(xvect, na.rm=T), labels=F) {
   x.cat <- cut(xvect, breaks=breaks, labels=labels)
}