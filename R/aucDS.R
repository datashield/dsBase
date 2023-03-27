#'
#' @title aucDS an aggregate function called by ds.auc
#' @description This function calculates the C-statistic or AUC (Area under the curve).
#' The current version can be used only for logistic regressions.
#' @details The AUC determines the discriminative ability of a model.
#' @param pred the name of the vector of the predicted values
#' @param y the name of the outcome variable. Note that this variable should include 
#' the complete cases that are used in the regression model.
#' @return returns the AUC and its standard error
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
aucDS <- function(pred=pred, y=y){
  
  if(is.character(pred)){
    pred <- eval(parse(text = pred), envir = parent.frame())
  }
  if(is.character(y)){
    y <- eval(parse(text = y), envir = parent.frame())
  }
  
  y <- as.numeric(as.character(y))
  
  n <- length(pred)
  n1 <- sum(y)
  mean.rank <- mean(rank(pred)[y == 1])
  AUC <- (mean.rank - (n1 + 1)/2)/(n - n1)
  n0 <- n-n1
  q0 <- AUC*(1-AUC)
  q1 <- AUC/(2-AUC)-AUC^2
  q2 <- 2*AUC^2/(1+AUC)-AUC^2
  se <- sqrt((q0+(n0-1)*q1+(n1-1)*q2)/(n0*n1))
  
  return(list(AUC=AUC,se=se))
  
}