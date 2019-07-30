#'
#' @title table2DDS.o (aggregate function) called by ds.table2D.o
#' @description This function generates a 2-dimensional contingency table where potentially disclosive cells
#' (based on a set threshold) are replaced by a missing value ('NA').
#' @details It generates 2-dimensional contingency tables where valid (non-disclosive) tables are defined 
#' as those where none of their cells have counts between 1 and the set threshold "nfilter.tab". When the ouput table
#' is invalid all cells except the total counts are replaced by missing values. Only the total counts are visible 
#' on the table returned to the client side. A message is also returned with the 2-dimensional table; the message 
#' says "invalid table - invalid counts present" if the table is invalid and 'valid table' otherwise.
#' @param xvect a numerical vector with discrete values - usually a factor.
#' @param yvect a numerical vector with discrete values - usually a factor.
#' @return a list which contains two elements: 'table', the 2-dimensional table and 'message' a message which
#' informs about the validity of the table.
#' @author Amadou Gaye, Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
table2DDS.o <- function(xvect,yvect){

  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS.o()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm<-as.numeric(thr$nfilter.glm)
  #nfilter.subset<-as.numeric(thr$nfilter.subset)
  #nfilter.string<-as.numeric(thr$nfilter.string)
  #nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)
  #nfilter.kNN<-as.numeric(thr$nfilter.kNN)
  #############################################################

  # tabulate the input vector and output the result in a data frame format
  aa <- table(xvect, yvect)
  bb <- matrix(NA, nrow=dim(aa)[1], ncol=dim(aa)[2])
  for(i in 1:dim(aa)[1]){
    bb[i,] <- aa[i,]
  }
  bb <- rbind(bb, colSums(bb))
  bb <- cbind(bb, rowSums(bb))
  cc <- as.data.frame(bb)
  colnames(cc) <- c(levels(as.factor(yvect)), "Total")
  rownames(cc) <- c(levels(as.factor(xvect)), "Total")

  # the minimum non-zero number of observations that is allowed in a single cell
  nfilter <- nfilter.tab
  
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
#AGGREGATE FUNCTION
# table2DDS.o
