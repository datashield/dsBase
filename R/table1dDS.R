#'
#' @title Creates 1-dimensional contingency tables - potentially disclosive data are suppressed
#' @description The function table1d.ds is a server-side subfunction of datashield.aggregate(). 
#' It generates 1-dimensional tables for all data sources. Valid (non-disclosive) data are defined 
#' as data from sources where no table cells have counts between 1 and 4 (the upper value [4] can in 
#' principle be changed but only by rewriting the underlying function - it cannot be changed by a 
#' standard DataSHIELD user). If the count in any cell in the table produced by a given data source 
#' IS invalid, that cell count is changed to "-1" and the name of the category that corresponds to it is 
#' changed to "-9". Each source is flagged as having only valid data, or at least some invalid data. 
#' Missing data are treated as na.action=na.omit (default for table function), if missing data are to be treated as
#' a separate and visible category, the variable should first be transformed to convert NAs to the a new value.
#' @param xvect a numerical vector with discrete values - usually a factor
#' @return A list object (eg. named out.obj) with one component from each separate study which contains the following items:
#' \item{is.table.valid}{eg. out.obj[[2]]$is.table.valid is a logical (Boolean) indicator of whether the data in study 2 is 
#' entirely valid (TRUE) for the stated variable or at least one category is invalid (FALSE)}
#' \item{safe.table}{eg. out.obj[[1]]$safe.table contains the safe (non-disclosive) one dimensional table that can be released 
#' from study 1 - unsafe data are concealed by converting invalid counts to "-1" and their corresponding catregory identifier to "-9"}
#' @author Burton, P.
#' @export
#'
table1dDS  <- function(xvect){

  # get the character name of the variable
  var.name.1_os2=deparse(substitute(xvect))

  # assign the input variable 
  factor_is1 <- xvect
  
  # assign name
  var.name.1_is1 <- var.name.1_os2              
 
  # reset to K+1 if the non-allowable range is changed to 1 to K
  critical.min <- 5 
  
  factor.valid <- TRUE
  
  table.new <- table(factor_is1)
  
  for(j in 1:length(table.new))
  {
    if(table.new[j]<critical.min && table.new[j] > 0)
    {
      table.new[j] <- (-1)
      factor.valid <- FALSE
      names(table.new)[j] <- (-9)
    }
  }
  names(dimnames(table.new)) <- paste("Categories/Counts for Variable:\n",var.name.1_is1)
  names(factor.valid) <- paste(var.name.1_is1)
  out.list_os1 <- list(factor.valid,as.table(table.new))
  names(out.list_os1) <- list("is.factor.valid","safe.table")
  
  # returns 1 key output object
  return(out.list_os1)  
}

