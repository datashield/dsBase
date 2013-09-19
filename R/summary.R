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
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=list("LAB_HDL"))
#'
#' # summary of the numerical vector 'LAB_HDL'
#' datashield.aggregate(opals, quote(summary(D$LAB_HDL)))
#'}
#'
summary <- function(data) {
  if(is.atomic(data)) {
    if(length(data) <= 1) {
      "Vector too small."
    } else {
      base::summary(data);
    }
  } else if(is.recursive(data)) {
    base::summary.default(data);
  }
}