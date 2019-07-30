#' 
#' @title Converts a numeric vector into a factor
#' @description This function is an assign DataSHIELD function that converts a numeric vector into
#' a factor type that presented as a vector or as a matrix with dummy variables.   
#' @details The functions converts the input variable into a factor which is presented as a vector
#' if the \code{fixed.dummy.vars} is set to FALSE or as a matrix with dummy variables if the 
#' \code{fixed.dummy.vars} is set to TRUE (see the help file of ds.asFactor.b for more details).
#' @param input.var.name the name of the variable that is to be converted to a factor.
#' @param all.unique.levels.transmit the levels that the variable will be transmitted to.  
#' @param fixed.dummy.vars a boolean that determines whether the new object will be represented as
#' a vector or as a matrix of dummy variables indicating the factor level of each data point.
#' If this argyment is set to FALSE (default) then the input variable is converted to a factor and
#' assigned as a vector. If is set to TRUE then the input variable is converted to a factor but 
#' assigned as a matrix of dummy variables. 
#' @param baseline.level a number indicating the baseline level to be used in the creation of the 
#' matrix of dummy variables. 
#' @return an object of class factor
#' @export
#'
asFactorDS2.o <- function(input.var.name=NULL, all.unique.levels.transmit=NULL, fixed.dummy.vars=NULL, baseline.level=NULL){

  input.var <- eval(parse(text=input.var.name))

  code.input <- all.unique.levels.transmit
  code.c <- unlist(strsplit(code.input, split=","))
  levels.var <- code.c

  if(fixed.dummy.vars==FALSE){
    factor.obj <- factor(input.var,levels=levels.var)
  }else{
    
    dummy.matrix <- matrix(NA,nrow=length(input.var),ncol=length(levels.var))
    varnames.list <- NULL

    for(g in 1:length(levels.var)){
      varnames.list <- c(varnames.list,paste0("DV",levels.var[g]))
      dummy.matrix[,g] <- as.numeric(input.var==levels.var[g])
    }
    
    dimnames(dummy.matrix) <- list(NULL,varnames.list)
    non.baseline.levels <- (1:length(levels.var))
    non.baseline.levels <- non.baseline.levels[non.baseline.levels!=baseline.level]
    factor.obj <- dummy.matrix[,non.baseline.levels]       
  }

  return(factor.obj)

}
#ASSIGN FUNCTION
# asFactorDS2.o
