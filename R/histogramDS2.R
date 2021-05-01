#' 
#' @title Computes a histogram of the input variable without plotting.
#' @description This function produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with number
#' of counts less than the pre-specified disclosure control set for the minimum cell
#' size of a table. If a bin has less counts than this threshold then their counts 
#' and its density are replaced by a 0 value.
#' @details Please find more details in the documentation of the clientside ds.histogram function.
#' @param xvect the numeric vector for which the histogram is desired.
#' @param num.breaks the number of breaks that the range of the variable is divided.
#' @param min a numeric, the lower limit of the distribution.
#' @param max a numeric, the upper limit of the distribution.
#' @param method.indicator a number equal to either 1, 2 or 3 indicating the method of disclosure
#' control that is used for the generation of the histogram. If the value is equal to 1 then the
#' 'smallCellsRule' is used. If the value is equal to 2 then the 'deterministic' method is used.
#' If the value is set to 3 then the 'probabilistic' method is used.
#' @param k the number of the nearest neghbours for which their centroid is calculated if the 
#' \code{method.indicator} is equal to 2 (i.e. deterministic method).
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the \code{method.indicator} is equal to 3 (i.e. probabilistic method).
#' @return a list with an object of class \code{histogram} and the number of invalid cells
#' @author Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
histogramDS2 <- function (xvect, num.breaks, min, max, method.indicator, k, noise){

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
  
  if (method.indicator==1){

    # Check if the number of breaks meets the DataSHIELD privacy criteria (disclosure control for
    # saturation)
    if (num.breaks > (nfilter.levels * length(xvect))){
      studysideMessage <- "FAILED: Number of breaks is too big. It may be disclosive - please shorten"
      stop(studysideMessage, call. = FALSE)
    }else{
      # breaks
      brks <- seq(from=min, to=max, by=(max-min)/num.breaks)
    } 
	
    # Get the histogram object
    histout <- graphics::hist(xvect, breaks=brks, plot=FALSE)
  
    # Check if the counts in each 'bin' are more than the disclosure setting for the minimum size
    # of table's cells
    indx <- which(histout$counts > 0 & histout$counts < nfilter.tab)
    
    # get the number of invalid cells
    invalidcells <- length(indx)
    
    if(invalidcells > 0){
      # Replace the corresponding counts and densities by zeros
      histout$counts[indx] <- 0
      histout$density[indx] <- 0   
    }
  }
  
  if(method.indicator==2){
    
    # Load the RANN package to use the 'nn2' function that searches for the Nearest Neighbours  
    # library(RANN)
    
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
    
    # Check if the number of breaks meets the DataSHIELD privacy criteria (disclosure control for
    # saturation)
    if (num.breaks > (nfilter.levels * length(xvect))){
      studysideMessage <- "FAILED: Number of breaks is too big. It may be disclosive - please shorten"
      stop(studysideMessage, call. = FALSE)
    }else{
      # breaks
      brks <- seq(from=min, to=max, by=(max-min)/num.breaks)
    } 
    
    # Get the histogram object
    histout <- graphics::hist(x.new, breaks=brks, plot=FALSE)
    invalidcells <- 0
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
    set.seed(seed)
    
    # generate the noise-augmented vector
    x.new <- x + stats::rnorm(N.data, mean=0, sd=sqrt(percentage*stats::var(x)))
    
    # Check if the number of breaks meets the DataSHIELD privacy criteria (disclosure control for
    # saturation)
    if (num.breaks > (nfilter.levels * length(xvect))){
      studysideMessage <- "FAILED: Number of breaks is too big. It may be disclosive - please shorten"
      stop(studysideMessage, call. = FALSE)
    }else{
      # breaks
      brks <- seq(from=min, to=max, by=(max-min)/num.breaks)
    } 
    
    # Get the histogram object
    histout <- graphics::hist(x.new, breaks=brks, plot=FALSE)
    invalidcells <- 0
  }  
    
  # Return a list with the histogram object and the number of invalid cells
  return(list("histobject"=histout, "invalidcells"=invalidcells))
  
}
# AGGREGATE FUNCTION
# histogramDS2
