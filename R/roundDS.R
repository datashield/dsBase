#'
#' @title roundDS
#' @description Generates objects from a server-side object, which can be either a vector or 
#' a data-frame column. Supports five operations:  
#' 1. (\code{round})  
#' 2. (\code{ceiling})  
#' 3. (\code{floor})
#' 4. (\code{trunc})
#' 5. (\code{signif})
#' where each function in baseR is applied on the server side to the specified object.
#' 
#' @details
#' Note: \code{add.column = TRUE} is only valid for data-frame inputs.
#' 
#' 
#' @param x Character vector specifying the server-side object(s). For data-frame columns, use the format \code{df$column}. 
#' @param type Character string specifying the operation: \code{"round"}, \code{"ceiling"}, \code{"floor"},
#' \code{trunc}, or \code{"signif"}.
#' @param digits Number of digits to be used in arguments \code{"round"} and \code{"signif"}.
#' @param add.column Logical. If \code{FALSE}, the result is created as a new server-side object; 
#' if \code{TRUE}, the result is added as a new column in the existing data-frame. Default is \code{FALSE}.
#' @param newobj Character string for the name of the object that will be created on the server. Default is \code{"rounding.result"}.
#' 
#' 
#' @author Zulal Bekerecioglu
#' @return the created numeric vector or the updated dataframe with the added column
#' @export
#'
#'
roundDS <- function(x, type, digits, add.column, newobj) {
  
  # If x is NULL, throw and error.
  if (is.null(x)) {
    stop("Input object couldn't be found, please provide an object for rounding in the correct format. 
          For vectors, supply an existing object name; for columns, use the format dataframe$column, 
          where 'dataframe' is the name of the data frame and 'column' is the column name, 
          and ensure that both exist.", call. = FALSE)
  }
  
  add.column <- as.logical(add.column)
  
  # Check if object is a column (contains $), if it is then save the dataframe name for later use if necessary
  if(grepl("\\$", x)) {
    is_column <- TRUE
    dataframe_name <- strsplit(x, "\\$")[[1]][1]
    column_name <- strsplit(x, "\\$")[[1]][2]
  } else {
    is_column <- FALSE
  }
  
  
  if(!is.null(x)&&!(is_column))
  {
    object <- get(x)
    
  } else if(!is.null(x)&&is_column){
    df <- get(dataframe_name)
    object <- df[[column_name]] 
  }
  
  result <- switch(type,
                    round   = round(object, digits = digits),
                    ceiling = ceiling(object),
                    floor   = floor(object),
                    trunc   = trunc(object),
                    signif  = signif(object, digits = digits))
  
  if(!(is_column)){ # if the object was a numerical vector, save the result in a new object
    return(result)
  } else if((is_column)&&!add.column){  # if the object was a column and add.column is FALSE, save the result in a new object
    return(result)
  } else if((is_column)&&add.column){ # if the object was a column and add.column is TRUE, save the result as a column
    df[[newobj]] <- result
    return(df)
  }
  
}
