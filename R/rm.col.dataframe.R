#' Deletes a column from a dataframe
#' 
#' @param data a dataframe 
#' @param col the column to delete
#' @export
#' 

rm.col.dataframe <- function (data, col) {
  tx <- a.dataframe[,-col]
}
