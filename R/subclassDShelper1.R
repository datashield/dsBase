#' 
#' @title generates subsets vectors from a factor vector
#' @description This is an internal function called by the function 'subclassDS'.
#' @details The function generates subsets if the input of 'subclassDS' is a factor vector.
#' @param xvect a vector of type factor.
#' @param nfilter the minimum number observation (i.e. rows) that are allowed.
#' @return a list which contains the subsets.
#' @author Gaye, A.
#'
.subclassDShelper1 <- function(xvect=NULL, filter=NULL){
  vectname <- xvect
  subsets <- list()
  names.of.subsets <- c()
  classes <- levels(D)
  for(i in 1:length(classes)){
    indices <- which(D == as.numeric(classes[i]))    
    if(!(length(indices) > 0 & length(indices) < filter)){
      subsets[[i]] <- D[indices]
      name.of.subD <- paste(vectname,".level_", classes[i], sep="")
      names.of.subsets <- append(names.of.subsets, name.of.subD)
    }else{
      invalidity <- 1
      subsets[[i]] <- rep(NA, length(indices))
      name.of.subD <- paste(vectname,".level_", classes[i], "_INVALID", sep="")
      names.of.subsets <- append(names.of.subsets, name.of.subD)
    }
    names(subsets) <- names.of.subsets
    output <- subsets
  }
  return(output)
}