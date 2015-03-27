#'
#' @title Creates a data frame
#' @description This function is similar to the R base function 'data.frame'.
#' @details an empty data frame is generated if the sought data frame is not valid.
#' @param vectors a list which contains the vectors to combine.
#' @param r.names NULL or a character vector specifying the names of the rows.
#' @param ch.rows logical, if TRUE then the rows are checked for consistency of length and names.
#' @param ch.names logical, logical. If TRUE then the names of the variables in the data frame 
#' are checked to ensure that they are syntactically valid variable names and are not duplicated. 
#' @param clnames a list of charcaters, the column names of the output data frame.
#' @param strAsFactors logical, tells if character vectors should be converted to factors?
#' @param completeCases a boolean that tells if only complete cases should be included or not.
#' @return a dataframe
#' @author Gaye, A.
#' @export
#' 
dataframeDS <- function (vectors=NULL,r.names=NULL,ch.rows=FALSE,ch.names=TRUE,clnames=NULL,strAsFactors=TRUE,completeCases=FALSE) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  if(strAsFactors){
    strAsFactors <- default.stringsAsFactors()
  }
  if(!(is.null(r.names))){
    r.names <- unlist(r.names)
  }
  
  dtemp <- data.frame(vectors, row.names=r.names, check.rows=ch.rows, check.names=ch.names, 
                    stringsAsFactors=strAsFactors)
  colnames(dtemp) <-  unlist(clnames)
  if(completeCases){
    dt <- dtemp[complete.cases(dtemp),]
  }else{
    dt <- dtemp
  }
  
  # check if the resulting dataframe is valid and output accordingly
  if(dim(dt)[1] < nfilter){
    if(dim(dt)[1] == 0){
      dt1 <- dt[-c(1:dim(dt)[1]),]
      dt <- dt1
    }else{
      dt[] <- NA
    }
  }
  
  return(dt)
  
}
