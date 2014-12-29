#'
#' @title Creates 2-dimensional contingency tables
#' @description This function generates a 2-dimensional table where potentially disclosive cells
#' (based on the set threshold) are replaced by a missing value ('NA').
#' @details It generates a 2-dimensional tables where valid (non-disclosive) 2-dimensional tables are defined 
#' as data from sources where no table cells have counts between 1 and the set threshold. When the ouput table
#' is invalid all cells but the total counts are replaced by missing values. Only the total counts are visible 
#' on the table returned to the client site. A message is also returned with the 2-dimensional table; the message 
#' says "invalid table - invalid counts present" if the table is invalid and 'valid table' otherwise.
#' @param xvect a numerical vector with discrete values - usually a factor.
#' @param yvect a numerical vector with discrete values - usually a factor.
#' @return a list which contains two elements: 'table', the 1-dimensional table and 'message' a message which
#' informs about the validity of the table.
#' @author Gaye A.
#' @export
#' 
table2dDS <- function(xvect,yvect){

  # tabulate the input vector and output the result in a data frame format
  aa <- table(xvect, yvect)
  bb <- matrix(NA, nrow=dim(aa)[1], ncol=dim(aa)[2])
  for(i in 1:dim(aa)[1]){
    bb[i,] <- aa[i,]
  }
  bb <- rbind(bb, colSums(bb))
  bb <- cbind(bb, rowSums(bb))
  cc <- as.data.frame(bb)
  colnames(cc) <- c(levels(yvect), "Total")
  rownames(cc) <- c(levels(xvect), "Total")

  # the minimum number of observations that are allowed (the below function gets the value from opal)
  nfilter <- setFilterDS()
  
  # check for invalid cells if any found change them to 'NA' and set the validity message accordingly
  validity <- "valid Table"
  for(i in 1: dim(cc)[2]){
    indx <- which(cc[1:(dim(cc)[1] - 1),i] > 0 & cc[1:(dim(cc)[1] - 1),i] < nfilter)
    if(length(indx) > 0){
      cc[1:(dim(cc)[1] - 1), 1:(dim(cc)[2] - 1)] <- NA
      validity <- "invalid table - invalid counts present"
      break
    }    
  }

  # return output table and message
  return(list(table=cc, message=validity))
}

