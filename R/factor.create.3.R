#' Generates a factor variable 
#' 
#' @param x a categorical variable (numerical or character)
#' @param maxval highest factor value
#' @param minval lowest factor value
#' @param Xval1 optional value to specify 
#' @param Xval2 optional value to specify 
#' @param Xval3 optional value to specify 
#' @param Xval4 optional value to specify 
#' @param Xval5 optional value to specify 
#' @param levels optional vector of values that 'x' might have taken
#' @param labels labels for the different levels
#' @param exclude values to be excluded
#' @param ordered tells if the variable 'x' is ordered
#' 
#' @export
#' 

factor.create.3 <- function (x = character(), maxval=NA, minval=NA,
          Xval1=NA, Xval2=NA, Xval3=NA, Xval4=NA, Xval5=NA,
          levels, labels = levels, exclude = NA, 
          ordered = is.ordered(x)) 
{
  if (is.null(x)) 
    x <- character()
  nx <- names(x)
  if (missing(levels)) {
    y <- unique(x)
    ind <- sort.list(y)
    y <- as.character(y)
    levels <- unique(y[ind])
  }
  if(is.na(maxval)==0 && is.na(minval)==1) {
    levels<-0:maxval
  }
  
  if(is.na(maxval)==0 && is.na(minval)==0) {
    levels<-minval:maxval
  }
  
  if(is.na(Xval1)==0){levels<-c(levels,Xval1)}
  if(is.na(Xval2)==0){levels<-c(levels,Xval2)}
  if(is.na(Xval3)==0){levels<-c(levels,Xval3)}
  if(is.na(Xval4)==0){levels<-c(levels,Xval4)}
  if(is.na(Xval5)==0){levels<-c(levels,Xval5)}
  
  
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
  else paste(labels, seq_along(levels), sep = "")
  class(f) <- c(if (ordered) "ordered", "factor")
  levels.all.valid<-
    sum((as.vector(summary(f))<5)*(as.vector(summary(f))>0))==0
  
  output.object<-NULL    
  if(levels.all.valid==TRUE){
    output.object<-f
  }
  output.object
  
}
