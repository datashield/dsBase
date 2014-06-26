#' 
#' @title Computes a histogram of the given data values without plotting.
#' @description This functions produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with a
#' count of less than 5. If a bin has a count < 5 it is collapsed with 
#' the nearing bin; this process iterates until all bins have count >=5.
#' @param xvect the numeric vector for which the histogram is desired.
#' @param min a numeric, the lower limit of the distribution.
#' @param max a numeric, the upper limit of the distribution.
#' @param seed an integer, the value to set the seed at when generating the break points.
#' @return a list with an object of class \code{histogram} and the number of invalid cells
#' @export
#' @author Gaye, A.
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(opal)
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # get the range of the input vector
#' cally <- call("range.ds", quote(D$LAB_TSC))
#' ranges <- datashield.aggregate(opals, cally)
#' minrs <- c()
#' maxrs <- c()
#' for(i in 1:length(ranges)){
#'   minrs <- append(minrs, ranges[[i]][1])
#'  maxrs <- append(maxrs, ranges[[i]][2])
#' }
#' range.arg <- c(min(minrs), max(maxrs))
#' seedval <- 333
#' generate a histogram object
#' cally <- call("histogram.ds", quote(D$LAB_TSC), range.arg[1], range.arg[2], seedval)
#' hist.object <- datashield.aggregate(opals, cally)
#' }
#' 
histogramDS <- function (xvect, min, max, seed) {
  
  # the same seed is set in each study to ensure all studies have the same break points
  set.seed(seed)
  
  # get the global break points and ensure that the breaks span the range of xvect.
  gotbreaks <- TRUE
  binwidth <- 0.3
  brks <- round(seq(min, max, by=binwidth),4)
  
  if(min(brks) > min || max(brks) < max){
    counter <- 0
    while(min(brks) > min || max(brks) < max){
      lastindx <- length(brks)
      brks <- c( (brks[1]-binwidth), brks, (brks[lastindx]+binwidth) )
      counter <- counter+1
      if(counter >= 50){
        gotbreaks <- FALSE
      }
    }
  }
  
  # generate the histogram object if the breaks points were obtained
  if(gotbreaks){
    # get the histogram object
    histout <- hist(xvect, breaks=brks, plot=FALSE)
    histout <- hist(xvect, breaks=brks, plot=FALSE)
    
    # check if any of the 'bins' contains a count < 5
    indx <- which(histout$counts > 0 & histout$counts < 5)
    l.small.counts <- length(indx)
    
    if(l.small.counts > 0){
  
      # replace the corresponding, counts, densities and intensities by zeros
      histout$counts[indx] <- 0
      histout$density[indx] <- 0   
      histout$intensities[indx] <- 0   
      
      # get the midpoints corresponding to the above indices
      # these midpoint correspond to the invalid categories
      invalidcells <- histout$mids[indx]
    }else{
      invalidcells <- NULL
    }
    
    # return a list with the histogram object and the vector 'axterispos'
    return(list("histobject"=histout, "invalidcells"=length(invalidcells)))
  }else{
    return(NULL)
  }
  
}
