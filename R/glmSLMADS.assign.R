#' @title Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)
#' @description Fits a generalized linear model (GLM) on data from single or multiple sources
#' with pooled co-analysis across studies being based on SLMA (Study Level Meta Analysis).
#' @details glmSLMADS.assign is an assign function called by clientside function ds.glmSLMA.
#' ds.glmSLMA also calls two aggregate functions glmSLMADS1 and glmSLMADS2.
#' For more detailed information see help for ds.glmSLMA.
#' @param formula a glm formula, specified in call to ds.glmSLMA
#' @param family a glm family, specified in call to ds.glmSLMA
#' @param offsetName a character string specifying a variable to be used as an offset.
#' Specified in call to ds.glmSLMA.
#' @param weightsName a character string specifying a variable to be used as regression weights.
#' Specified in call to ds.glmSLMA. Specified in call to ds.glmSLMA.
#' @param dataName a character string specifying the name of a data.frame
#' holding the data for the model. Specified in call to ds.glmSLMA.
#' @return writes glm object summarising the fitted model to the serverside.
#' For more detailed information see help for ds.glmSLMA.
#' @author Paul Burton for DataSHIELD Development Team (14/7/20)
#' @export
glmSLMADS.assign <- function(formula, family, offsetName, weightsName, dataName){

  # Convert transmitable text for special link variance combinations back to full representation
  if(family=="quasigamma.link_log")
  {family<-"quasi(link=log,variance=mu^2)"}

  if(family=="Gamma.link_log")
  {family<-"Gamma(link=log)"}

  # Correctly name offset, weights and data objects in function call
  # (to allow glmPredict to work correctly later)
  calltext <- paste0("mg<-glm(formula,family=",family,",offset=",
             offsetName,",weights=",weightsName,",data=", dataName,",x=TRUE)")

  eval(parse(text=calltext))

  # update the call object to include the actual formula
  mg$call$formula <- formula

  return(mg)

}
# ASSIGN FUNCTION
# glmSLMADS.assign
