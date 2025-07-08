#' 
#' @title generates subsets vectors from a factor vector
#' @description This is an internal function called by the function 'subsetByClassDS'.
#' @details The function generates subsets if the input of 'subsetByClassDS' is a factor vector.
#' @param xvect a vector of type factor.
#' @param xname the name of the vector.
#' @param filter the minimum number observation (i.e. rows) that are allowed.
#' @return a list which contains the subsets.
#' @keywords internal
#' @noRd
#' @author Gaye, A.
#'
subsetByClassHelper1 <- function(xvect=NULL, xname=NULL, filter=NULL){
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
        subsets[[i]] <-  xvect[-c(1:length(xvect))]
        name.of.subD <- paste(vectname,".level_", categories[i], "_EMPTY", sep="")
      }else{
        temp1 <- xvect[indices]
        temp1[1:length(temp1)] <- NA
        subsets[[i]] <- temp1
        name.of.subD <- paste(vectname,".level_", categories[i], "_INVALID", sep="")
      }
      names.of.subsets <- append(names.of.subsets, name.of.subD)
    }
    names(subsets) <- names.of.subsets
    output <- subsets
  }
  return(output)
}
