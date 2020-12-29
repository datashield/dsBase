.is.df.column.suitable <- function(variable, variable.name)
{
  outcome <- FALSE
  if(is.data.frame(variable) & length(variable.name) == 2)
  {
     column.exists <- any(variable.name[2] %in% colnames(variable))
     if (column.exists)
     {
       types   <- lapply(variable, typeof)
      outcome <- types[variable.name[2]] %in% c("integer", "numeric","double")
     }
  }
  return(outcome)
}

.is.list.suitable <- function(variable)
{
  outcome <- FALSE
  if (is.list(variable))
  {
    var.vector <- unlist(variable)
    outcome <- is.numeric(var.vector) & length(var.vector) > 1
  }
  return(outcome)
}

.is.array.suitable <- function(variable)
{
  outcome <- FALSE
  if(is.array(variable))
  {
    outcome <- is.numeric(variable) & (sum(dim(variable)) > 2)
  }
  return(outcome)
}

.is.matrix.suitable <- function(variable)
{
  outcome <- FALSE
  if(is.matrix(variable))
  {
    outcome <- is.numeric(variable) & (sum(dim(variable)) > 2)
  }
  return(outcome)
}

.is.vector.suitable <- function(variable)
{
  outcome <- FALSE
  if(is.vector(variable))
  {
    outcome <- is.numeric(variable) & length(variable) > 1
  }
  return(outcome)
}

#' @name  isNumVariableValidDS
#' @title checks if a variable or a column of data.frame exists on a DataSHIELD server of type numeric
#' @param x the variable name
#' @return TRUE - if a numerical variable (or column of a dataframe exists). Otherwise, FALSE
#' @keywords server.function
#' @export isNumVariableValidDS
#'
 isNumVariableValidDS <- function(x = "")
{
  outcome = FALSE
  if (is.character(x))
  {

      #split the variable name and the colunm name of a data frame.
      #The name of the variable is always the first element of the vector
      variable.name    <- unlist(strsplit(x = x, split = "$", fixed = TRUE))

      #check the variable exists
      if(exists(variable.name[1], where = 1))
      {

        #get the variable to be tested
        variable       <- get(variable.name[1], pos = 1)
        count <- 1

        while(!outcome & count < 6)
        {
          outcome <- switch(
                 count,
                .is.df.column.suitable(variable, variable.name), #dataframe
                .is.list.suitable(variable), #list
                .is.array.suitable(variable), #array
                .is.matrix.suitable(variable), #matrix
                .is.vector.suitable(variable) #vector
                )

          count <- count + 1
        }

      } # exists
  }
  return(outcome)
}
