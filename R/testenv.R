#' @title testenv
#' @description Thiggregate function called by ds.glmSLMA
#' @details It is an
#' aggregate function that sets up the generalized linear model structure
#' anduded in the study-level
#' meta-analysis. For more details please see the extensive header for ds.glmSLMA
#' in DataSHIELD and help on the {glm} function in native R.
#' @param dataName an optional character string specifying the name of a data.frame
#' object holding the data to be analysed under the specified model.
#' Fully specified by <dataName> argument in ds.glmSLMA
#' @return All 
#' @author Paul Burton for DataSHIELD Development Team
#' @export

testenv <- function(dataName){
  mystat = sys.status()
  return(mystat)
}