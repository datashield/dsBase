#'
#' @title Creates 1-dimensional contingency tables
#' @description This function generates a 1-dimensional table where potentially disclosive cells.
#' (based on the set threshold) are replaced by a missing value ('NA').
#' @details It generates a 1-dimensional tables where valid (non-disclosive) 1-dimensional tables are defined 
#' as data from sources where no table cells have counts between 1 and the set threshold. When the ouput table
#' is invalid all cells but the total count are replaced by missing values. Only the total count is visible 
#' on the table returned to the client site. A message is also returned with the 1-dimensional; the message 
#' says "invalid table - invalid counts present" if the table is invalid and 'valid table' otherwise.
#' @param xvect a numerical vector with discrete values - usually a factor.
#' @return a list which contains two elements: 'table', the 1-dimensional table and 'message' a message which
#' informs about the validity of the table.
#' @author Gaye A.
#' @export
#'
table1dDS  <- function(xvect){

  # tabulate the input vector and output the result in a data frame format
  aa <- t(as.data.frame((table(xvect))))
  bb <- as.data.frame(t(as.numeric(aa[2,]))) 
  
  # add the total count and colnames names
  cc <- cbind(bb, sum(bb[1,], na.rm=TRUE))
  colnames(cc) <- c(aa[1,], "Total")

  # the minimum number of observations that are allowed (the below function gets the value from opal)
  nfilter <- setFilterDS()
  
  # check for invalid cells if any found change them to 'NA' and set the validity message accordingly
  validity <- "valid Table"
  indx <- which(cc[1,1:(dim(cc)[2] - 1)] > 0 & cc[1,1:(dim(cc)[2] - 1)] < nfilter)
  if(length(indx) > 0){
    cc[1,1:(dim(cc)[2] - 1)] <- NA
    validity <- "invalid table - invalid counts present"
  }
  
  # return output table and message
  return(list(table=cc, message=validity))
}

