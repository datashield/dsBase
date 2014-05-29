#' 
#' @title Changes a reference level of a factor
#' @description This function is similar to R function \code{relevel}, 
#' but in addition addes numbering to the levels so that they are displayed 
#' in the right order when creating cross-tables.
#' @param xvect a factor
#' @param ref the reference level
#' @return  a factor of the same length as xvect
#' @author Isaeva, J., Gaye, A.
#' @export
#'
changeRefGroupDS <- function(xvect, ref=NULL){
  
  xvect_reordered = relevel(xvect, ref)
  xvect_levels = levels(xvect_reordered)
  for (i in 1:length(xvect_levels)) {
    dummy = as.character(i-1)
    # xvect_levels[i] = paste0(dummy, "_", xvect_levels[i])
    levels(xvect_reordered)[i] = paste0(dummy, "_", levels(xvect_reordered)[i])
  }
  
  # check if the output vector is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(xvect)
  
  # check if the output is valid and output accordingly
  if(!check){
    if(length(xvect_reordered == 0)){
      xvect_reordered <- NA
    }else{
      xvect_reordered <- rep(NA, length(xvect_reordered))
    }
  }
  return(xvect_reordered)
  
}