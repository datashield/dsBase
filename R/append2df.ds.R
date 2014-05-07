#'
#' @title Appends a column to a dataframe
#' @description append a column to a dataframe and creates a new dataframe.
#' @param dataset a dataframe
#' @param xvect the vector to append to the dataframe
#' @return a new dataframe 
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(opal)
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a new variable (e.g. a mean centered LAB_HDL)
#' # get the mean of LAB_HDL
#' mean.lab_hdl <- datashield.aggregate(opals, quote(mean.ds(D$LAB_HDL)))
#' # center LAB_HDL for each study
#' for(i in 1:length(opals)){
#'   call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
#'   datashield.assign(opals[i], "lab_hdl.c", call.object)
#' }
#' 
#' # now append 'lab_hdl.c' to the initially assigned dataframe 'D'
#' # without replacing the initial dataframe 'D'
#' call.object <- call("append2dfDS", quote(D), quote(lab_hdl.c))
#' datashield.assign(opals, call.object)
#' }
#'
append2df.ds <- function (dataset, xvect) {
  
  if(is.data.frame(dataset)){
    new.dataset <- cbind(dataset, xvect)
    old.colnames <- colnames(dataset)
    colnames(new.dataset) <- c(old.colnames, as.character(quote(xvect)))
    return(new.dataset)
  }else{
    stop("\n\'dataset' must be a dataframe!\n\n")
  }
  
}
