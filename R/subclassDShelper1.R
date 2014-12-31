#' 
#' @title generates subsets vectors from a factor vector
#' @description This is an internal function called by the function 'subclassDS'.
#' @details The function generates subsets if the input of 'subclassDS' is a factor vector.
#' @param xvect a vector of type factor.
#' @param xname the name of the vector.
#' @param nfilter the minimum number observation (i.e. rows) that are allowed.
#' @return a list which contains the subsets.
#' @keywords internal
#' @author Gaye, A.
#'
subclassDShelper1 <- function(xvect=NULL, xname=NULL, filter=NULL){
  vectname <- xname
  subsets <- list()
  names.of.subsets <- c()
  categories <- levels(xvect)
  for(i in 1:length(categories)){
    indices <- which(xvect == as.numeric(categories[i]))    
    if(!(length(indices) < filter)){
      subsets[[i]] <- xvect[indices]
      name.of.subD <- paste(vectname,".level_", categories[i], sep="")
      names.of.subsets <- append(names.of.subsets, name.of.subD)
    }else{
      # if any one category has between 0 and 'filter' observation turn subset content into missing values
      if(length(indices) == 0){
        subsets[[i]] <- factor(NULL, levels=categories[i])
        name.of.subD <- paste(vectname,".level_", categories[i], "_EMPTY", sep="")
      }else{
        subsets[[i]] <- factor(rep(NA,length(indices)), levels=categories[i])
        name.of.subD <- paste(vectname,".level_", categories[i], "_INVALID", sep="")
      }
      names.of.subsets <- append(names.of.subsets, name.of.subD)
    }
    names(subsets) <- names.of.subsets
    output <- subsets
  }
  return(output)
}