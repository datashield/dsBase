#' 
#' @title Generates a matrix with a minimum and maximum value, for two numerical vector
#' @description This function used by another function to plot a heatmap graph.
#' @details the minumum and maximum values are potentially disclosive particularly in the presence
#' of 'outliers'. However although this function generates the two extreme values of a vector, 
#' it is not not disclosive because the minimum and maximum that are return are, each, multiplied
#' by a very small random value from a uniform distribution. That way the 'exact' minimum and maximum
#' are not returned but at the same them the limits are not sensibly altered.
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @return a matrix
#' @author Isaeva, J. and Gaye A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC")
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate the 'blured' minimum and maximum of the variable 'LAB_TSC'
#' datashield.aggregate(opals, quote(densitygrid.ds(D$LAB_TSC)))
#' }
#' 
MinMax.ds <- function(xvect, yvect) {
  
  MinMaxMatrix <- data.frame(cbind(c(0,0),c(0,0)))
  colnames(MinMaxMatrix) <- c("xvect", "yvect")
  rownames(MinMaxMatrix) <- c("min", "max")
  
  MinMaxMatrix[1, 1] <- min(xvect, na.rm=T)
  MinMaxMatrix[2, 1] <- max(xvect, na.rm=T)
  MinMaxMatrix[1, 2] <- min(yvect, na.rm=T)
  MinMaxMatrix[2, 2] <- max(yvect, na.rm=T)
  
  MinMaxMatrix[1, 1] <- runif(1, 0.9, 1) * MinMaxMatrix[1, 1]
  MinMaxMatrix[2, 1] <- runif(1, 1, 1.1) * MinMaxMatrix[2, 1] 
  MinMaxMatrix[1, 2] <- runif(1, 0.9, 1) * MinMaxMatrix[1, 2]
  MinMaxMatrix[2, 2] <- runif(1, 1, 1.1) * MinMaxMatrix[2, 2]
  
  return(MinMaxMatrix)  
}
