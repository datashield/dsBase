#' 
#' @title dataFrameFillDS
#' @description An assign function called by the clientside ds.dataFrameFill function.
#' @details This function checks if each study has all the variables compared to the other studies
#' in the analysis. If a study does not have some of the variables, the function generates those
#' variables as vectors of missing values and combines them as columns to the input data frame. 
#' Then, the "complete" in terms of the columns dataframe is saved in each server with a name
#' specified by the argument \code{newobj} on the clientside. 
#' @param df.name a character string representing the name of the input data frame that will be
#' filled with extra columns with missing values if a number of variables is missing from it
#' compared to the data frames of the other studies used in the analysis.
#' @param allNames.transmit unique names of all the variables that are included in the input 
#' data frames from all the used datasources. 
#' @param class.vect.transmit the classes of all the variables that are included in the vector 
#' \code{allNames.transmit}.
#' @param levels.vec.transmit the levels of all factor variables.
#' The classes supported are 'numeric', 'integer', 'character', 'factor' and 'logical'.
#' @return Nothing is returned to the client. The generated object is written to the serverside.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
dataFrameFillDS <- function(df.name, allNames.transmit, class.vect.transmit, levels.vec.transmit){
  
  data <- eval(parse(text=df.name), envir = parent.frame())

  if(!is.null(allNames.transmit)){
    allNames <- unlist(strsplit(allNames.transmit, split=","))
  }else{
    allNames <- NULL
  }
  
  if(!is.null(class.vect.transmit)){
    class.vect <- unlist(strsplit(class.vect.transmit, split=","))
  }else{
    class.vect <- NULL
  }
  
  if(!is.null(levels.vec.transmit)){
    levels.vec <- strsplit(levels.vec.transmit, split=" ")
    levels.vec <- lapply(levels.vec, function(x){as.numeric(unlist(strsplit(x, split=",")))})
  }else{
    levels.vec <- NULL
  }

  for (class.vect.index in 1:length(class.vect)){
      if (! class.vect[class.vect.index] %in% c('numeric', 'integer', 'character', 'factor' , 'logical')){
         stop(paste0("Unexpected missing class specified: '", class.vect[class.vect.index], "'"), call.=FALSE)
      }
  }
  
  study.colnames <- colnames(data)
  missingIndex <- which(!(allNames %in% study.colnames))
  
  missingFactorsIndex <- which(class.vect=='factor')
  if(!is.null(levels.vec)){
    missingFactorsIndex.vec <- rep(NA,length(allNames))
    for (i in 1:length(missingFactorsIndex.vec)){
      if(class.vect[[i]]=='factor'){
        missingFactorsIndex.vec[i] <- i
      }else{
        missingFactorsIndex.vec[i] <- NA
      }  
    }
    nonNA.missingFactorsIndex <- length(missingFactorsIndex)

    Factorslevels <- as.list(rep(NA, length(allNames)))
    for (i in 1:nonNA.missingFactorsIndex){
        Factorslevels[[missingFactorsIndex[i]]] <- levels.vec[[i]]
    } 
  }  
  
  if (length(missingIndex) > 0){
    missingVars <- allNames[missingIndex]
    missingClass <- class.vect[missingIndex]
    numRows <- nrow(data)
    numCols <- length(missingVars)
  
    mat.new <- matrix(NA, ncol=numCols, nrow=numRows)
  
    df.new <- data.frame(x=mat.new, row.names=NULL)
    colnames(df.new) <- missingVars
  
    funs <- sapply(paste0("as.", missingClass), match.fun)
    df.new[] <- Map(function(dd, f) f(as.character(dd)), df.new, funs)

    if(length(which(missingClass=='factor')) > 0){
      missingLevels <- Factorslevels[missingIndex]
      factor.index <- which(missingClass=='factor')
      for(i in 1:length(factor.index)){
        levels(df.new[,factor.index[i]]) <- missingLevels[[factor.index[i]]]
      }
    }
      
    df.out <- cbind(data, df.new)
  }else{
    df.out <- data
  }
  
  return(df.out)

}
# ASSIGN FUNCTION
# dataFrameFillDS

