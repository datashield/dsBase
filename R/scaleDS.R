#'
#' @title scaleDS
#' @description Generates scaled objects using a server-side object, which can be either a vector or 
#' a data-frame column. 
#' 
#' @details
#' Note: \code{add.column = TRUE} is only valid for data-frame inputs.
#' 
#' 
#' @param x Character string specifying the server-side vector For data-frame columns, use the format \code{df$column}. 
#' @param newobj Character string for the name of the object that will be created on the server. Default is \code{"scaled.data"}.
#' @param add.column Logical. If \code{FALSE}, the result is created as a new server-side object; 
#' if \code{TRUE}, the result is added as a new column in the existing data-frame. Default is \code{FALSE}.
#' 
#' @author Zulal Bekerecioglu
#' @return the created numeric vector or the updated dataframe with the added column
#' @export
#'
#'
scaleDS <- function(x=NULL, newobj=NULL, add.column=NULL) { 
  
  add.column <- as.logical(add.column)
  
  error_message <- "Input object couldn't found, please provide the correct format. For vectors, supply an existing object name; 
                    for columns, use df$colname and ensure the dataframe and column exist."
  
  # If x is NULL, throw and error.
  if (is.null(x)) {
    stop(error_message, call. = FALSE)
  } else {
    is_dataframe <- grepl("\\$", x)
    
    if(is_dataframe) {
      # Extract dataframe name 
      dataframe_name <- strsplit(x, "\\$")[[1]][1]
      column_name <- strsplit(x, "\\$")[[1]][2]
      
      df <-get(dataframe_name)
    } else {
      
      object <-get(x)
    }
  }
  
  
  if(is_dataframe) {
    result <- as.numeric(scale(df[[column_name]])) # scale the column
  } else {
    result <- as.numeric(scale(object)) # scale the vector
  }
  
  # Return the dataframe with the added column, or the new object.
  if(is_dataframe&&add.column) {
    df[[newobj]] <- result
    return(df)
  } else {
    return(result)
  }
  
  
}
