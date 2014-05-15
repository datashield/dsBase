#' 
#' @title Computes covariance between two or more vectors
#' @description this function is similar to R function 'cov'
#' @param x a character, the name of a vector, dataframe or matrix
#' @param y (optional) a character, the name of a vector, dataframe or matrix
#' @return covariance
#' @author Gaye, A.
#' @export
#' 
covDS <-function (x=NULL, y=NULL){
  
  # check what vectors are factors and turn them into numeric
  if(class(x)=='matrix' | class(x)=='data.frame'){
    cls <- colnames(x)
    rc <- c()
    for(i in 1:dim(x)[2]){
      rc <- append(rc, class(x[,i]))
    }
    idx <- which(rc == 'factor')
    if(length(idx) > 0){
      x[, idx] <- sapply(x[, idx], as.character)
      x[, idx] <- sapply(x[, idx], as.numeric)
    }
    output <- cov(x)
  }else{
    if(class(x)=='factor'){
      x <- as.character(x)
      x <- as.numeric(x)
    }
    if(class(y)=='factor'){
      y <- as.character(y)
      y <- as.numeric(y)
    }
    output <- cov(x,y)
  }
  
  return(output)
}
