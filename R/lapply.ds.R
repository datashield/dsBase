#' 
#' @title Applies a function to each of the given categories
#' @details The function requires a numeric and a factor variable.
#' The numeric values are categorised according to the factor levels.
#' Two functions \code{mean.ds} and \code{var.ds} can then be applied 
#' each category. 
#' @param xvect a numeric vector
#' @param yvect a factor vector
#' @param fun a list that holds the charcater name of the function to apply.
#' The functions to apply are restricted to \code{mean.ds} and \code{var.ds}.
#' @return a list which contains the results of the applied function 
#' for each category.
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' library(opal)
#' myvar <- list("PM_BMI_CATEGORICAL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the mean for the BMI categories that correspond to the two GENDER levels
#' call.object <- call("lapply.ds", quote(D$LAB_TSC), quote(D$GENDER), quote(list("mean.ds")))
#' datashield.aggregate(opals, call.object)
#'
#' # compute the variance for the BMI categories that correspond to the two GENDER levels
#' call.object <- call("lapply.ds", quote(D$LAB_TSC), quote(D$GENDER), quote(list("var.ds")))
#' datashield.aggregate(opals, call.object)
#'  }
#'
lapply.ds <- function (xvect=NULL, yvect=NULL, fun=NULL){
  
  if(is.null(xvect)){
    stop("\n\nNo numeric vector provided (check 'xvect')!\n\n")
  }else{
    if(!(is.numeric(xvect))){
      stop("\n\nxvect must be numeric vector!\n\n")
    }
  }
  
  if(is.null(yvect)){
    stop("\n\nNo factor vector provided (check 'yvect')!\n\n")
  }else{
    if(!(is.factor(yvect))){
      stop("\n\nxvect must be factor vector!\n\n")
    }
  }
  
  if(!(is.function(fun))){
    stop("\n\n'fun' must be the function 'mean.ds' or 'var.ds'\n\n")
  }
  
  dt <- data.frame(xvect, yvect)
  ll <- levels(yvect)
  results <- c()
  labels <- c()
  if(fun[[1]] == "mean.ds"){
    for(i in 1:length(ll)){
      x <- dt[which(dt[,2]==as.numeric(ll[i])), 1] 
      res <- mean.ds(x)
      results <- append(results, round((res),3))
    }
  }
  if(fun[[1]] == "var.ds"){
    for(i in 1:length(ll)){
      x <- dt[which(dt[,2]==as.numeric(ll[i])), 1] 
      res <- var.ds(x)
      results <- append(results, round((res),3))
    }
  }
  return(results)
}
  