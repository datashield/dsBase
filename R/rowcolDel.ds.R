#' 
#' @title Deletes rows or columns from a dataframe
#' @param dataset a dataframe
#' @param dimension an integer 1 to delete rows and 2 to delete columns
#' @param indx a numeric vector that represents the rows or columns to remove
#' @return a dataframe
#' @author Gaye, A.
#' @export
#' 
rowcolDel.ds <- function(dataset, dimension, indx){
  
  if(dimension == 1){
    out <- dataset[-indx,]
  }
  if(dimension == 2){
    out <- dataset[,-indx]    
  }

  return(out)
}