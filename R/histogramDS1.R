#' 
#' @title returns the minimum and the maximum of the input numeric vector
#' @description this function returns the minimum and maximum of the input numeric vector which
#' depends on the argument \code{method.indicator}. If the method.indicator is set to 1 (i.e. the 
#' 'smallCellsRule' is used) the computed minimum and maximum values are multiplied by a very small
#' random number. If the method.indicator is set to 2 (i.e. the 'deteministic' method is used) the 
#' function returns the minimum and maximum values of the vector with the scaled centroids. If the
#' method.indicator is set to 3 (i.e. the 'probabilistic' method is used) the function returns the
#' minimum and maximum values of the generated 'noisy' vector.
#' @param xvect the numeric vector for which the histogram is desired.
#' @param method.indicator a number equal to either 1, 2 or 3 indicating the method of disclosure
#' control that is used for the generation of the histogram. If the value is equal to 1 then the
#' 'smallCellsRule' is used. If the value is equal to 2 then the 'deterministic' method is used.
#' If the value is set to 3 then the 'probabilistic' method is used.
#' @param k the number of the nearest neghbours for which their centroid is calculated if the 
#' \code{method.indicator} is equal to 2 (i.e. deterministic method).
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the \code{method.indicator} is equal to 3 (i.e. probabilistic method).
#' @return a numeric vector which contains the minimum and the maximum values of the vector
#' @author Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
histogramDS1 <- function(xvect, method.indicator, k, noise){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr <- listDisclosureSettingsDS()                           #
  nfilter.tab <- as.numeric(thr$nfilter.tab)                  #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  #
  nfilter.noise <- as.numeric(thr$nfilter.noise)              #
  nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################  
  
  # print an error message if the input vector is not a numeric
  if(!(is.numeric(xvect))){
    output <- "The input vector is not a numeric!"
  }else{
    
    if (method.indicator==1){
      
      # the study-specific seed for random number generation
      seed <- getOption("datashield.seed")
      if (is.null(seed))
        stop("histogramDS1 requires 'datashield.seed' R option to operate", call.=FALSE)
      set.seed(seed)
      
      rr <- c(min(xvect, na.rm=TRUE), max(xvect, na.rm=TRUE))
      if(rr[1] < 0){ min <- rr[1] * stats::runif(1, 1.01, 1.05) }else{ min <- rr[1] * stats::runif(1, 0.95, 0.99) }
      if(rr[2] < 0){ max <- rr[2] * stats::runif(1, 0.95, 0.99) }else{ max <- rr[2] * stats::runif(1, 1.01, 1.05) }
    
      output <- c(min, max)
      
    }
    
    if(method.indicator==2){
      
      # Remove any missing values
      x <- stats::na.omit(xvect)
      
      # Standardise the variable
      x.standardised <- (x-mean(x))/stats::sd(x)
      
      # Calculate the length of the variable after ommitting any NAs
      N.data <- length(x)
      
      # Check if k is integer and has a value greater than or equal to the pre-specified threshold
      # and less than or equal to the length of rows of data.complete minus the pre-specified threshold
      if(k < nfilter.kNN | k > (N.data - nfilter.kNN)){
        stop(paste0("k must be greater than or equal to ", nfilter.kNN, " and less than or equal to ", (N.data-nfilter.kNN), "."), call.=FALSE)
      }else{
        neighbours = k
      }
      
      # Find the k-1 nearest neighbours of each data point
      nearest <- RANN::nn2(x.standardised, k = neighbours)
      
      # Calculate the centroid of each n nearest data points
      x.centroid <- matrix()
      for (i in 1:N.data){
        x.centroid[i] <- mean(x.standardised[nearest$nn.idx[i,1:neighbours]])
      }
      
      # Calculate the scaling factor
      x.scalingFactor <- stats::sd(x.standardised)/stats::sd(x.centroid)
      
      # Apply the scaling factor to the centroids
      x.masked <- x.centroid * x.scalingFactor
      
      # Shift the centroids back to the actual position and scale of the original data
      x.new <- (x.masked * stats::sd(x)) + mean(x)
      
      # find the minimum and the maximum of the distribution
      min <- min(x.new)
      max <- max(x.new)
      
      output <- c(min, max)
    
    }
    
    if(method.indicator==3){
      
      # Remove any missing values
      x <- stats::na.omit(xvect)
      
      # Calculate the length of the variable after ommitting any NAs
      N.data <- length(x)
      
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
        stop("histogramDS requires 'datashield.seed' R option to operate", call.=FALSE)
      set.seed(seed)
      
      # generate the noise-augmented vector
      x.new <- x + stats::rnorm(N.data, mean=0, sd=sqrt(percentage*stats::var(x)))
      
      # find the minimum and the maximum of the distribution
      min <- min(x.new)
      max <- max(x.new)
      
      output <- c(min, max)
      
    }
      
  }
  
  return (output)
  
}
# AGGREGATE FUNCTION
# histogramDS1
