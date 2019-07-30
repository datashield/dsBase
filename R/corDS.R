#' 
#' @title Computes correlation between two or more vectors
#' @description this function is similar to R function 'cor'
#' @param x a character, the name of a vector, dataframe or matrix
#' @param y (optional) a character, the name of a vector, dataframe or matrix
#' @param use a character string giving a method for computing covariances in the 
#' presence of missing values. This must be one of the strings: "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @return correlation
#' @author Gaye, A.
#' @export
#' 
corDS <-function (x=NULL, y=NULL, use=NULL){
  todo <- use
  # check what vectors are factors and turn them into numeric
  if(class(x)=='matrix' | class(x)=='data.frame'){
    rc <- c()
    for(i in 1:dim(x)[2]){
      rc <- append(rc, class(x[,i]))
    }
    idx <- which(rc == 'factor')
    if(length(idx) > 0){
      x[, idx] <- sapply(x[, idx], as.character)
      x[, idx] <- sapply(x[, idx], as.numeric)
    }
    output1 <- cor(x,use=todo)
    completeCount <- output1
    cls <- colnames(x)
    for(i in 1:dim(completeCount)[2]){
      for(j in 1:dim(completeCount)[2]){
        if(i == j){
          count <- length(which((complete.cases(x[,1])==TRUE)))
        }else{
          count <- length(which((complete.cases(cbind(x[,i], x[,j]))==TRUE)))
        }
        completeCount[i,j] <- round(count,0)
      }
    }
    output2 <- completeCount
    output <- list(output1, output2)
  }else{
    if(class(x)=='factor'){
      x <- as.character(x)
      x <- as.numeric(x)
    }
    if(class(y)=='factor'){
      y <- as.character(y)
      y <- as.numeric(y)
    }
    output1 <- cor(x,y,use=todo)
    output2 <- count <- length(which((complete.cases(cbind(x, y))==TRUE)))
    output <- list(output1, output2)
  }
  
  return(output)
}