#'
#' @title Creates a data frame
#' @description This function is similar to the R base function 'data.frame'.
#' @details an empty data frame is generated if the sought data frame is not valid.
#' @param vectors a list which contains the vectors to combine.
#' @param row.names NULL or a character vector specifying the names of the rows.
#' @param ch.rows logical, if TRUE then the rows are checked for consistency of length and names.
#' @param ch.names logical, logical. If TRUE then the names of the variables in the data frame 
#' are checked to ensure that they are syntactically valid variable names and are not duplicated. 
#' @param strAsFactors logical, tells if character vectors should be converted to factors?
#' @return a dataframe
#' @author Gaye, A.
#' @export
#' 
#'
dataframeDS <- function (vectors=NULL, r.names=NULL, ch.rows=FALSE, ch.names=TRUE, strAsFactors=TRUE) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  if(strAsFactors){
    strAsFactors <- default.stringsAsFactors()
  }
  row.names <- unlist(row.names)
  
  dt <- data.frame(vectors, row.names=r.names, check.rows=ch.rows, check.names=ch.names, 
                    stringsAsFactors=strAsFactors)
  
  # check if the resulting dataframe is valid and output accordingly
  if(dim(dt)[1] < nfilter){
    if(dim(dt)[1] == 0){
      dt1 <- as.data.frame(matrix(NA,nrow=1, ncol=dim(dt)[2]))
      colnames(dt1) <- colnames(dt)
      dt <- dt1
    }else{
      dt <- as.data.frame(matrix(NA,nrow=dim(dt)[1], ncol=dim(dt)[2]))
    }
  }
  
  return(dt)
  
}
