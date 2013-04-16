#' Computes a grid-density object required to generate a contour plot.
#'
#' @param xvect a numerical vector 
#' @param yvect a numerical vector
#' @return a heatmap plot.
#' @author Burton, P.
#' @export
#' 
contour.plot.1 <- function(xvect,yvect)
{
  #par(mfrow=c(1,1))
  y.min<-min(yvect)
  x.min<-min(xvect)
  y.max<-max(yvect)
  x.max<-max(xvect)
  
  
  y.range<-max(yvect)-min(yvect)
  x.range<-max(xvect)-min(xvect)
  
  numints<-20
  y.interval<-y.range/numints
  x.interval<-x.range/numints
  
  y.cuts<-seq(from=y.min,to=y.max,by=y.interval)
  y.mids<-seq(from=(y.min+y.interval/2),to=(y.max-y.interval/2),by=y.interval)
  y.cuts[numints+1]<-y.cuts[numints+1]*1.001
  
  x.cuts<-seq(from=x.min,to=x.max,by=x.interval)
  x.mids<-seq(from=(x.min+x.interval/2),to=(x.max-x.interval/2),by=x.interval)
  x.cuts[numints+1]<-x.cuts[numints+1]*1.001
  
  
  grid.density<-matrix(0,nrow=numints,ncol=numints)
  
  for(j in 1:numints)
  {
    for(k in 1:numints)
    {
      grid.density[j,k]<-sum(1*(yvect>=y.cuts[j] & yvect<y.cuts[j+1] & xvect >=x.cuts[k] & xvect<x.cuts[k+1]))
    }
  }
  print(length(x.mids))
  print(length(y.mids))
  
  #grid.density.obj<-data.frame(matrix(grid.density),as.vector(x.mids),as.vector(y.mids))
  grid.density.obj<-cbind(grid.density,x.mids,y.mids)
  
  return(grid.density.obj)
  
  
}
