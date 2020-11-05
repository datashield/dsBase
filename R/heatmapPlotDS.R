#' 
#' @title Calculates the coordinates of the centroid of each n nearest neighbours
#' @description This function calculates the coordinates of the centroids for each n nearest neighbours.
#' @details The function finds the n-1 nearest neighbours of each data point in a 2-dimensional space.
#' The nearest neighbours are the data points with the minimum Euclidean distances from the point of 
#' interest. Each point of interest and its n-1 nearest neighbours are then used for the calculation
#' of the coordinates of the centroid of those n points. Centroid here is referred to the centre of mass,
#' i.e. the x-coordinate of the centroid is the average value of the x-coordinates of the n nearest
#' neighbours and the y-coordinate of the centroid is the average of the y-coordinates of the n nearest
#' neighbours. The coordinates of the centroids return to the client side function and can be used for the
#' plot of non-disclosive graphs (e.g. scatter plots, heatmap plots, contour plots, etc).    
#' @param x the name of a numeric vector, the x-variable.
#' @param y the name of a numeric vector, the y-variable.
#' @param k the number of the nearest neghbours for which their centroid is calculated if the 
#' \code{method.indicator} is equal to 1 (i.e. deterministic method).
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the \code{method.indicator} is equal to 2 (i.e. probabilistic method).
#' @param method.indicator a number equal to either 1 or 2. If the value is equal to 1 then the
#' 'deterministic' method is used. If the value is set to 2 the 'probabilistic' method is used. 
#' @return a list with the x and y coordinates of the centroids if the deterministic method is used
#' or the x and y coordinated of the noisy data if the probabilistic method is used.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
heatmapPlotDS <- function(x, y, k, noise, method.indicator){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr <- listDisclosureSettingsDS()                           #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  #
  nfilter.noise <- as.numeric(thr$nfilter.noise)              #
  #nfilter.levels <- as.numeric(thr$nfilter.levels)           #
  #############################################################
  
  # Cbind the columns of the two variables and remove any rows that include NAs
  data.table <- cbind.data.frame(x, y)
  data.complete <- stats::na.omit(data.table)
  
  x <- as.vector(data.complete[,1])
  y <- as.vector(data.complete[,2])
  
  if(method.indicator==1){
    
    # standardise the variables
    x.standardised <- (x-mean(x))/stats::sd(x)
    y.standardised <- (y-mean(y))/stats::sd(y)
    
    # Create a data.frame for the variables
    data <- data.frame(x.standardised, y.standardised)
    
    # Calculate the length of the data.frame after ommitting any rows with NAs 
    N.data <- dim(data)[1]
    
    # Check if k is integer and has a value greater than or equal to the pre-specified threshold 
    # and less than or equal to the length of rows of data.complete minus the pre-specified threshold
    if(k < nfilter.kNN | k > (N.data - nfilter.kNN)){
      stop(paste0("k must be greater than or equal to ", nfilter.kNN, "and less than or equal to ", (N.data-nfilter.kNN), "."), call.=FALSE)
    }else{
      neighbours = k
    }
    
    # Find the k-1 nearest neighbours of each data point 
    nearest <- RANN::nn2(data, k = neighbours)
    
    # Calculate the centroid of each n nearest data points 
    x.centroid <- matrix()
    y.centroid <- matrix()
    for (i in 1:N.data){
      x.centroid[i] <- mean(x.standardised[nearest$nn.idx[i,1:neighbours]])
      y.centroid[i] <- mean(y.standardised[nearest$nn.idx[i,1:neighbours]])
    }
    
    # Calculate the scaling factor
    x.scalingFactor <- stats::sd(x.standardised)/stats::sd(x.centroid)
    y.scalingFactor <- stats::sd(y.standardised)/stats::sd(y.centroid)
    
    # Apply the scaling factor to the centroids
    x.masked <- x.centroid * x.scalingFactor
    y.masked <- y.centroid * y.scalingFactor
    
    # Shift the centroids back to the actual position and scale of the original data
    x.new <- (x.masked * stats::sd(x)) + mean(x)
    y.new <- (y.masked * stats::sd(y)) + mean(y)
    
  }
  
  if (method.indicator==2){
    
    # Create a data.frame for the variables
    data <- data.frame(x, y)
    
    # Calculate the length of the data.frame after ommitting any rows with NAs 
    N.data <- dim(data)[1]
    
    # Check if the percentage of the variance that is specified in the argument 'noise'
    # and is used as the variance of the embedded noise is a greater
    # than the minimum threshold specified in the filter 'nfilter.noise'
    if(noise < nfilter.noise){
      stop(paste0("'noise' must be greater than or equal to ", nfilter.noise), call.=FALSE)
    }else{
      percentage <- noise
    }
    
    # the study-specific seed for random number generation
    seed <- getOption("datashield.seed")
    if (is.null(seed))
      stop("heatmapPlotDS requires 'datashield.seed' R option to operate", call.=FALSE)
    set.seed(seed)
    x.new <- x + stats::rnorm(N.data, mean=0, sd=sqrt(percentage*stats::var(x)))
    y.new <- y + stats::rnorm(N.data, mean=0, sd=sqrt(percentage*stats::var(y)))
    
  }
  
  # Return a list with the x and y coordinates of the centroids
  return(list(x.new, y.new))
  
}
# AGGREGATE FUNCTION
# heatmapPlotDS
