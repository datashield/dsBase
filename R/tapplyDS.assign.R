#' @title tapplyDS.assign called by ds.tapply.assign
#' @description Apply one of a selected range of functions to summarize an
#' outcome variable over one or more indexing factors and write the resultant
#' summary as a newobj on the serverside
#' @details see details for \code{ds.tapply.assign} function
#' @param X.name, the name of the variable to be summarized. Specified
#' via argument <X.name> of \code{ds.tapply.assign} function
#' @param INDEX.names.transmit, the name of a single factor or a vector of names of factors to
#' index the variable to be summarized. Specified via argument <INDEX.names>
#' of \code{ds.tapply.assign} function
#' @param FUN.name, the name of one of the allowable summarizing functions to be applied.
#' Specified via argument <FUN.name> of {ds.tapply.assign} function.
#' @return an array of the summarized values created by the \code{tapplyDS.assign} function. This
#' array is written as a newobj on the serverside. It has the same number of dimensions as INDEX.
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
tapplyDS.assign <- function(X.name, INDEX.names.transmit, FUN.name){
  
  # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  
  if(is.character(X.name)){
    X <- eval(parse(text=X.name), envir = parent.frame())
  }else{
    studysideMessage <- "ERROR: X.name must be specified as a character string"
    stop(studysideMessage, call. = FALSE)
  }
  
  INDEX.factors <- unlist(strsplit(INDEX.names.transmit, split=","))
  
  num.factors <- length(INDEX.factors)
  
  # test that output vector and indexing factor all have the same length
  length.2.test <- length(X)
  
  length.test.vector <- rep(NA, num.factors)
  
  for(g in 1:num.factors){
    activation.text.0 <- paste0("INDEX.factors[",g,"]")
    active.factor.name <- eval(parse(text=activation.text.0))
    active.factor <- eval(parse(text=active.factor.name), envir = parent.frame())
    length.test.vector[g] <- length(active.factor)
  }
  
  for(h in 1:num.factors){
    if(length.2.test!=length.test.vector[h]){
      return.message <- "Error: the output variable and all indexing factors must be of equal length"
      stop(return.message, call. = FALSE)
    }  
  }
  
  # convert INDEX.names format from transmittable to actionable form (a list of vectors)
  INDEX.names.list <- paste0("list(",INDEX.names.transmit,")")
  INDEX <- eval(parse(text=INDEX.names.list), envir = parent.frame())
  
  # select complete cases on X and all INDEX factors only
  df <- as.data.frame(cbind(X, do.call(cbind, INDEX)))
  colnames(df) <- c(X.name, INDEX.factors)
  df.complete <- df[stats::complete.cases(df),]
  
  X.complete <- df.complete[,X.name]
  INDEX.complete <- df.complete[,c(INDEX.factors)]
  
  INDEX.complete.list <- list()
  if(num.factors > 1){
    for(i in 1:ncol(INDEX.complete)) {
      INDEX.complete.list[[i]] <- INDEX.complete[,i]
    }
    names(INDEX.complete.list) <- colnames(INDEX.complete) 
    INDEX <- INDEX.complete.list
  }else{
    INDEX <- list(INDEX.complete)
  }   
  
  N.count <- tapply(X.complete, INDEX, base::length)
  
  #################
  #Valid functions#
  #################
  
  # N
  if(FUN.name=="N" || FUN.name=="length" || FUN.name=="Length" || FUN.name=="LENGTH"){
    
    # make output neat if up to two INDEX factors
    if(num.factors==1){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      dimnames(N.count)[[1]] <- factor1.level.names
    }
    
    if(num.factors==2){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      factor2.levels <- levels(factor(INDEX[[2]]))
      factor2.level.names <- factor2.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      for(v in 1:length(factor2.levels)){
        factor2.level.names[v] <- paste0(INDEX.factors[2], ".", factor2.levels[v])
      }
      dimnames(N.count)[[1]] <- factor1.level.names
      dimnames(N.count)[[2]] <- factor2.level.names
    }
    
    output <- list(N=N.count)
    return(output)
  }
  
  
  # MEAN
  if(FUN.name=="mean" || FUN.name=="Mean" || FUN.name=="MEAN"){
    
    Mean <- tapply(X.complete, INDEX, base::mean)
    
    # make output neat if up to two INDEX factors
    if(num.factors==1){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      dimnames(Mean)[[1]] <- factor1.level.names
      dimnames(N.count)[[1]] <- factor1.level.names
    }
    
    if(num.factors==2){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      factor2.levels <- levels(factor(INDEX[[2]]))
      factor2.level.names <- factor2.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      for(v in 1:length(factor2.levels)){
        factor2.level.names[v] <- paste0(INDEX.factors[2], ".", factor2.levels[v])
      }
      dimnames(Mean)[[1]] <- factor1.level.names
      dimnames(Mean)[[2]] <- factor2.level.names
      dimnames(N.count)[[1]] <- factor1.level.names
      dimnames(N.count)[[2]] <- factor2.level.names
    }
    
    output <- list(Mean=Mean, N=N.count)
    return(output)
  }
  
  # SD
  if(FUN.name=="sd" || FUN.name=="SD"){
    
    SD <- tapply(X.complete, INDEX, stats::sd)
    
    # make output neat if up to two INDEX factors
    if(num.factors==1){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      dimnames(SD)[[1]] <- factor1.level.names
      dimnames(N.count)[[1]] <- factor1.level.names
    }
    
    if(num.factors==2){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      factor2.levels <- levels(factor(INDEX[[2]]))
      factor2.level.names <- factor2.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      for(v in 1:length(factor2.levels)){
        factor2.level.names[v] <- paste0(INDEX.factors[2], ".", factor2.levels[v])
      }
      dimnames(SD)[[1]] <- factor1.level.names
      dimnames(SD)[[2]] <- factor2.level.names
      dimnames(N.count)[[1]] <- factor1.level.names
      dimnames(N.count)[[2]] <- factor2.level.names
    }
    
    output <- list(SD=SD, N=N.count)
    return(output)
  }
  
  # SUM
  if(FUN.name=="sum" || FUN.name=="Sum" || FUN.name=="SUM"){
    
    Sum <- tapply(X.complete, INDEX, base::sum)
    
    # make output neat if up to two INDEX factors
    if(num.factors==1){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      dimnames(Sum)[[1]] <- factor1.level.names
      dimnames(N.count)[[1]] <- factor1.level.names
    }
    
    if(num.factors==2){
      factor1.levels <- levels(factor(INDEX[[1]]))
      factor1.level.names <- factor1.levels
      factor2.levels <- levels(factor(INDEX[[2]]))
      factor2.level.names <- factor2.levels
      for(u in 1:length(factor1.levels)){
        factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
      }
      for(v in 1:length(factor2.levels)){
        factor2.level.names[v] <- paste0(INDEX.factors[2], ".", factor2.levels[v])
      }
      dimnames(Sum)[[1]] <- factor1.level.names
      dimnames(Sum)[[2]] <- factor2.level.names
      dimnames(N.count)[[1]] <- factor1.level.names
      dimnames(N.count)[[2]] <- factor2.level.names
    }
    
    output <- list(Sum=Sum, N=N.count)
    return(output)
  }
  
  
  # QUANTILE
  if(FUN.name=="quantile" || FUN.name=="Quantile" || FUN.name=="QUANTILE"){
    
    if(num.factors > 1){
      studysideMessage <- "Quantile will only work with one indexing factor but you can combine several factors into one. e.g. two factors with f1 and f2 levels respectively can be combined into one with f1 x f2 levels"
      stop(studysideMessage, call. = FALSE)
    }
    probs.vector <- c(0.05,0.1,0.2,0.25,0.3,0.33,0.4,0.5,0.6,0.67,0.7,0.75,0.8,0.9,0.95)
    Quantile <- tapply(X.complete, INDEX, stats::quantile, probs=probs.vector)
    
    factor1.levels <- levels(factor(INDEX[[1]]))
    factor1.level.names <- factor1.levels
    for(u in 1:length(factor1.levels)){
      factor1.level.names[u] <- paste0(INDEX.factors[1], ".", factor1.levels[u])
    }
    dimnames(Quantile)[[1]] <- factor1.level.names
    
    return(Quantile)
  }
  
  return.message <- "Please specify a valid DataSHIELD tapply function e.g. FUN.name='mean' or FUN.name='sd'"
  return(return.message)
  
}
# ASSIGN.FUNCTION
# tapplyDS.assign
