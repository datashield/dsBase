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

factor.create.3 <- function (x = base::character(), maxval=NA, minval=NA,
          Xval1=NA, Xval2=NA, Xval3=NA, Xval4=NA, Xval5=NA,
          levels, labels = levels, exclude = NA, 
          ordered = base::is.ordered(x)) 
{
  if (base::is.null(x)) 
    x <- base::character()
  nx <- base::names(x)
  if (base::missing(levels)) {
    y <- base::unique(x)
    ind <- base::sort.list(y)
    y <- base::as.character(y)
    levels <- base::unique(y[ind])
  }
  if(base::is.na(maxval)==0 && base::is.na(minval)==1) {
    levels<-0:maxval
  }
  
  if(base::is.na(maxval)==0 && base::is.na(minval)==0) {
    levels<-minval:maxval
  }
  
  if(base::is.na(Xval1)==0){levels<-c(levels,Xval1)}
  if(base::is.na(Xval2)==0){levels<-c(levels,Xval2)}
  if(base::is.na(Xval3)==0){levels<-c(levels,Xval3)}
  if(base::is.na(Xval4)==0){levels<-c(levels,Xval4)}
  if(base::is.na(Xval5)==0){levels<-c(levels,Xval5)}
  
  
  base::force(ordered)
  exclude <- base::as.vector(exclude, base::typeof(x))
  x <- base::as.character(x)
  levels <- levels[base::is.na(match(levels, exclude))]
  f <- base::match(x, levels)
  if (!base::is.null(nx)) 
    base::names(f) <- nx
  nl <- base::length(labels)
  nL <- base::length(levels)
  if (!base::any(nl == c(1L, nL))) 
    base::stop(base::gettextf("invalid labels; length %d should be 1 or %d", 
                  nl, nL), domain = NA)
  base::levels(f) <- if (nl == nL) 
    base::as.character(labels)
  else base::paste(labels, base::seq_along(levels), sep = "")
  base::class(f) <- c(if (ordered) "ordered", "factor")
  levels.all.valid<-
    base::sum((base::as.vector(base::summary(f))<5)*(base::as.vector(base::summary(f))>0))==0
  
  output.object<-NULL    
  if(levels.all.valid==TRUE){
    output.object<-f
  }
  output.object
  
}
