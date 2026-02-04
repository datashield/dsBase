#'
#' @title dateDS
#' @description Takes an object that is either a data-frame column or a vector, and can do extraction of 
#' components of full date (\code{extractdate}), can combine date components to a full date (\code{makedate}), 
#' or can calculate the time between two dates (\code{timebetween}).
#' 
#' @details 
#' If the input is a data-frame column, it must be provided in the \code{x} argument as data-frame$column. 
#' Inputs for \code{extractdate} and \code{timebetween} must be date objects.  
#' For \code{makedate}, three numeric vectors (year, month, day) must be provided in the correct order.  
#' The \code{add.column} argument determines whether the result is added as a new column in the existing 
#' data-frame (\code{TRUE}), or created as a new server-side object (\code{FALSE}).  
#' Note: \code{add.column = TRUE} is only valid for data-frame inputs.
#' 
#' 
#' @param x Character vector specifying the server-side object(s). For data-frame columns, use the format \code{df$column}. 
#' @param type Character string specifying the operation: \code{"extractdate"}, \code{"makedate"}, or \code{"timebetween"}.
#' @param newobj Character string for the name of the object that will be created on the server. Default is \code{"date.result"}.
#' @param unit Character string specifying the unit for \code{extractdate} or \code{timebetween}: \code{"days"}, \code{"months"}, or \code{"years"}.
#' @param add.column Logical. If \code{FALSE}, the result is created as a new server-side object; 
#' if \code{TRUE}, the result is added as a new column in the existing data-frame. Default is \code{FALSE}.
#' 
#' 
#' @author Zulal Bekerecioglu
#' @return the created numeric vector or date object, or the updated dataframe with the added column
#' @export
#'
#'
dateDS <- function(x=NULL, type=NULL,
                   newobj=NULL, unit=NULL, add.column=NULL) { 
  
  library(lubridate)
  add.column <- as.logical(add.column)
  
  # If argument not in c("extractdate", "makedate", "timebetween") throw an error
  if (!(type %in% c("extractdate", "makedate", "timebetween"))) {
    stop("Invalid argument. Must be one of: ", paste(c("extractdate", "makedate", "timebetween"), collapse=", "))
  }
  
  # Check if input(s) are valid.
  error_message <- "Input object couldn't be found. Please provide the correct format. 
                    For vectors, supply an existing object name; for columns, use the format dataframe$column, 
                    where 'dataframe' is the name of the data frame and 'column' is the column name, 
                    and ensure that both exist."
  
    
  # If x is NULL, throw and error.
  if (is.null(x)) {
    stop(error_message, call. = FALSE)
  }
    
  # Each argument takes a unique number of elements, check if they match.
  if (length(x) != 1 && type=="extractdate") {
    stop(paste0("Invalid input length for argument '", type, "'. Expected ", 1, 
      " elements, but received ", length(x), ". Please provide exactly ", 1, 
      " object name(s) or column(s)."), call. = FALSE)
  }
    
  if (length(x) != 3 && type=="makedate") {
    stop(paste0("Invalid input length for argument '", type, "'. Expected ", 3, 
                     " elements, but received ", length(x), ". Please provide exactly ", 3, 
                     " object name(s) or column(s)."), call. = FALSE)
  }
    
  if (length(x) != 2 && type=="timebetween") {
    stop(paste0("Invalid input length for argument '", type, "'. Expected ", 2, 
                     " elements, but received ", length(x), ". Please provide exactly ", 2, 
                     " object name(s) or column(s)."), call. = FALSE)
  }
    
  inputs <- vector("list", length(x))
  
  # When add.column = TRUE, the client function ensures that at least one input is a column,
  # and if multiple columns are provided, they all come from the same data-frame.
  # Therefore, it is safe to use any of these data-frames as common_df on the server.
  common_df <- NULL 
  
  for (i in seq_along(x)) {
    element <- x[i]
    
    # Each element must be a single string
    if (!is.character(element) || length(element) != 1) stop(error_message, call. = FALSE)
    
    # Check if it is a df$col reference
    if (grepl("\\$", element, perl = TRUE)) {
      parts <- strsplit(element, "\\$", perl = TRUE)[[1]]
      
      # Validate both parts
      if (length(parts) != 2 || !nzchar(parts[1]) || !nzchar(parts[2])) stop(error_message, call. = FALSE)
      
      # Try to get the dataframe
      df <- tryCatch(get(parts[1]), error = function(e) NULL)
      if (is.null(df)) stop(error_message, call. = FALSE)
      
      # Check that column exists
      if (!(parts[2] %in% names(df))) stop(error_message, call. = FALSE)
      
      # Save the column values in the list
      inputs[[i]] <- df[[parts[2]]]
      common_df <- parts[1]
      
    } else {
      # It's a plain object, just get it
      obj <- tryCatch(get(element), error = function(e) NULL)
      if (is.null(obj)) stop(error_message, call. = FALSE)
      
      inputs[[i]] <- obj
    }
  }
  
  
  # extractdate
  # x should be a column name or object with a date format, type should be provided, 
  # a new column or object with outputcolname will be created
  if (type == "extractdate") {
    
    # Only one input is expected
    date_input <- inputs[[1]]
    
    # Extract the requested component
    result <- switch(unit,
                     days   = day(date_input),
                     months = month(date_input),
                     years  = year(date_input),
                     stop("Invalid unit. Must be one of: days, months, years"))
  }
  
  
  # makedate
  # inputcolname should be list of 3 strings: year-month-day
  if (type == "makedate") {
    
    # inputs[[1]] = year, inputs[[2]] = month, inputs[[3]] = day
    year_vec  <- as.numeric(inputs[[1]])
    month_vec <- as.numeric(inputs[[2]])
    day_vec   <- as.numeric(inputs[[3]])
    
    # Basic plausibility checks
    if (length(unique(sapply(list(year_vec, month_vec, day_vec), length))) != 1) {
      stop("Inputs for 'makedate' must be of equal length.", call. = FALSE)
    }
    
    if (any(year_vec < 1000 | year_vec > 3000, na.rm = TRUE)) {
      stop("The 'year' input in 'makedate' must contain plausible 4-digit years (1000–3000).
           Check that year, month, and day are given in the correct order (year, month, day).", call. = FALSE)
    }
    if (any(month_vec < 1 | month_vec > 12, na.rm = TRUE)) {
      stop("The 'month' input in 'makedate' must contain values between 1 and 12.
           Check that year, month, and day are given in the correct order (year, month, day).", call. = FALSE)
    }
    if (any(day_vec < 1 | day_vec > 31, na.rm = TRUE)) {
      stop("The 'day' input in 'makedate' must contain values between 1 and 31.
           Check that year, month, and day are given in the correct order (year, month, day).", call. = FALSE)
    }
    
    
    result <- make_date(year  = year_vec,
                          month = month_vec,
                          day   = day_vec)
  }
  
  
  # timebetween
  # inputcolname should be a list of 2 strings: start and end date
  if (type == "timebetween") {
    
    # inputs[[1]] = start date, inputs[[2]] = end date
    units <- list(
      years  = years(1),
      months = months(1),
      days   = days(1)
    )
    
    result <- interval(inputs[[1]], inputs[[2]]) %/% units[[unit]]
  }
  
  # Save result based on add.column
  if (!add.column) {
    return(result)
  } else {
    # Assign to common_df as a new column
    df <- get(common_df)
    df[[newobj]] <- result
    return(df)
  }
  
}
