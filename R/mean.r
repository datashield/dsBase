#' Computes the mean statistic for vectors with more than 4 entries
#'
#' @param a
#' @export
#' 

mean <- function (a) {
  if(length(a) >= 5){
    mean(a,na.rm=TRUE)
  }else{
    cat("Operation not allowed!\n")
  }
}