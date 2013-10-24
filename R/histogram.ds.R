#' 
#' @title Computes a histogram of the given data values without plotting.
#' @description this functions produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with a
#' count of less than 5. If a bin has a count < 5 it is collapsed with 
#' the nearing bin; this process iterates until all bins have count >=5.
#' @param xvect the numeric vector for which the histogram is desired.
#' @param brks a string character where the break values are concatenated by ',';
#' this is because we can use the R function c() as an aggregate function.
#' @return a list with an object of class \code{histogram} and the number of invalid cells
#' @export
#' @author Gaye, A.
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#' library(opal)
#  myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a histogram object without plotting
#' hist.object <- datashield.aggregate(opals, quote(histogram.ds(D$LAB_TSC)))
#' }
#' 
histogram.ds <- function (xvect, brks) {
  
  # turn the brks point given as a character into numricals
  brks_n <- as.numeric(unlist(strsplit(brks, split=",")))
  
  # get the histogram object
  histout <- hist(xvect, breaks=brks_n, plot=FALSE)
  
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
  
}
