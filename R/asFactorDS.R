#' 
#' @title Turns a numeric vector into a factor vector
#' @description this function is similar to R function \code{as.factor} except for
#' the fact that it does not allow for levels with one observation only.
#' @param x  a vector of data, usually taking a small number of distinct values.
#' @param levels	an optional vector of the values (as character strings) that x might have taken.
#' @param labels	either an optional character vector of labels for the levels (in the same order 
#' as levels after removing those in exclude), or a character string of length 1.
#' @param exclude	a vector of values to be excluded when forming the set of levels. 
#' This should be of the same type as x, and will be coerced if necessary.
#' @param ordered	logical flag to determine if the levels should be regarded as ordered (in the order given).
#' @return an object of class 'factor' or NULL if the resulting factor is invalid
#' @author Burton, P.; Gaye, A.
#' @export
#' 
asFactorDS <-function (x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x)){
  
  if (is.null(x))
    x <- character()
  x.table<-table(x)
  invalid.table<-(sum((x.table<2)*1)>=1)*1
  
  nx <- names(x)
  if (missing(levels)) {
    y <- unique(x)
    ind <- sort.list(y)
    y <- as.character(y)
    levels <- unique(y[ind])
    
  }
  force(ordered)
  exclude <- as.vector(exclude, typeof(x))
  x <- as.character(x)
  levels <- levels[is.na(match(levels, exclude))]
  f <- match(x, levels)
  if (!is.null(nx))
    names(f) <- nx
  nl <- length(labels)
  nL <- length(levels)
  if (!any(nl == c(1L, nL)))
    stop(gettextf("invalid labels; length %d should be 1 or %d",
                  nl, nL), domain = NA)
  levels(f) <- if (nl == nL)
    as.character(labels)
  else paste0(labels, seq_along(levels))
  class(f) <- c(if (ordered) "ordered", "factor")
  
  if(invalid.table==0)
  {return(f)}
  
  if(invalid.table>0){
    f <- rep(NA, 4)
    levels(f) <- "NA"
    return(f)
  }
  
}
