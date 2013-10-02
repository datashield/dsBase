#'
#' @title Generates a summary output
#' @details This function is equivalent to the R base function 'summary'. 
#' Due to DataSHIELD microdata protection principle (1) atomic vector with length < 5 
#' are not allowed and (2)  the 'min' and 'max' are stripped out as they are potentially 
#' disclosive.
#' @param data some atomic or recursive data
#' @return a summary
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign a variable to R
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=list("LAB_HDL"))
#'
#' # summary of the numerical vector 'LAB_HDL'
#' datashield.aggregate(opals, quote(summary.ds(D$LAB_HDL)))
#'}
#'
summary.ds <- function(data) {
  if(is.atomic(data)) {
    if(length(data) < 5) {
      stop("The input vector is not valid, its length < 5!\n")
    } else {
      temp <- summary(data)
      # remove the 'min' and 'max' values which are potentially disclosive
      indxmin <- which(names(temp)=="Min.")
      indxmax <- which(names(temp)=="Max.")
      ss <- temp[-c(indxmin,indxmax)]
      return(ss)
    }
  } 
  
  if(is.recursive(data)) {
    ss <- summary.default(data)
    return(ss)
  }
}