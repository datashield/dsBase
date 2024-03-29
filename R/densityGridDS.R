#' 
#' @title Generates a density grid with or without a priori defined limits
#' @description Generates a density grid that can then be used for heatmap or countour plots.
#' @details Invalid cells (cells with count < to the set filter value for the minimum allowed
#' counts in table cells) are turn to 0.
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @param limits a logical expression for whether or not limits of the density grid are defined by
#' a user. If \code{limits} is set to "FALSE", min and max of xvect and yvect are used as a range.
#' If \code{limits} is set to "TRUE", limits defined by x.min, x.max, y.min and y.max are used.
#' @param x.min a minimum value for the x axis of the grid density object, if needed
#' @param x.max a maximum value for the x axis of the grid density object, if needed
#' @param y.min a minimum value for the y axis of the grid density object, if needed
#' @param y.max a maximum value for the y axis of the grid density object, if needed
#' @param numints a number of intervals for the grid density object, by default is 20
#' @return a grid density matrix
#' @author Julia Isaeva, Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
densityGridDS  <- function(xvect, yvect, limits=FALSE, x.min=NULL, x.max=NULL, y.min=NULL, y.max=NULL, numints=20){
  
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    
  thr <- dsBase::listDisclosureSettingsDS()				
  nfilter.tab <- as.numeric(thr$nfilter.tab)					
  #nfilter.glm <- as.numeric(thr$nfilter.glm)					
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)            
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)  
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  
  #############################################################
    
  xvect.save <- xvect
  yvect.save <- yvect
  
  xvect.not.missing <- 1-(is.na(xvect)*1)
  yvect.not.missing <- 1-(is.na(yvect)*1)
  
  x.and.y.present <- xvect.not.missing * yvect.not.missing
  
  xvect <- xvect.save[x.and.y.present==1]
  yvect <- yvect.save[x.and.y.present==1]
  
  if(limits==FALSE){ 
    if((!is.null(x.min)) | (!is.null(x.max)) | (!is.null(y.min)) | (!is.null(y.max))){
      stop('Density grid range should not be defined when variable limits is FALSE') 
	}else{
      y.min <- min(yvect)
      x.min <- min(xvect)
      y.max <- max(yvect)
      x.max <- max(xvect)
    }
  }else{
    if(limits==TRUE){
      if((is.null(x.min)) | (is.null(x.max)) | (is.null(y.min)) | (is.null(y.max))){
        stop('All ranges for density grid should be defined')
      }
	  }
  }	
  
  y.range <- y.max-y.min
  x.range <- x.max-x.min
  
  y.interval <- y.range/numints
  x.interval <- x.range/numints
  
  y.cuts <- seq(from=y.min, to=y.max, by=y.interval)
  y.mids <- seq(from=(y.min+y.interval/2), to=(y.max-y.interval/2), by=y.interval)
  y.cuts[numints+1] <- y.cuts[numints+1] * 1.001
  
  x.cuts <- seq(from=x.min, to=x.max, by=x.interval)
  x.mids <- seq(from=(x.min+x.interval/2), to=(x.max-x.interval/2), by=x.interval)
  x.cuts[numints+1] <- x.cuts[numints+1] * 1.001
  
  grid.density <- matrix(0, nrow=numints, ncol=numints)
  cell.count <- 0
  
  # check if the counts in each grid meet the DataSHIELD privacy criteria and
  # replace the grid.density to 0 if the counts are less than the filter value 
  for(j in 1:numints){
    for(k in 1:numints){
      grid.density[j,k] <- sum(1*(yvect >= y.cuts[k] & yvect < y.cuts[k+1] & xvect >= x.cuts[j] & xvect < x.cuts[j+1]), na.rm=TRUE)
      if((grid.density[j,k] > 0) & (grid.density[j,k] < nfilter.tab)){
        grid.density[j,k]  <- 0
        cell.count  <- cell.count + 1
      }
    }
  }
  
  grid.density.obj <- cbind(grid.density, x.mids, y.mids)
  
  title.text <- paste('Number of invalid cells (cells with counts >0 and < nfilter.tab ) is ', cell.count, sep='')
  
  names(dimnames(grid.density.obj))[2] <- title.text
  names(dimnames(grid.density.obj))[1] <- ''
  
  return(grid.density.obj)
  
}
# AGGREGATE FUNCTION
# densityGridDS
