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
  my_search = search()
  my_env = environment()
  par_env = parent.env(environment())
  fun_env = environment(glmSLMADS1)
  ls_gl = ls(globalenv())
  ls_loc = ls()
  ls_par_fr = ls(parent.frame())
  ls_par_env = ls(parent.env(environment()))
  sys_status = sys.status()
  outlist = list(my_search, my_env,par_env,fun_env, ls_gl, ls_loc, ls_par_fr,ls_par_env, sys_status)
  return(outlist)
}