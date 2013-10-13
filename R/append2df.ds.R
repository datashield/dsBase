#'
#' @title Appends a column(s) to a dataframe
#' @param dataset a dataframe
#' @param column a list which contains the column/vector to append to the dataframe
#' @param colname a character, the name of the appended column
#' @return a new dataframe is assigned to the datasources
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#' library(opal)
#  myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar, symbol="D")
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
#' # and replace the initial dataframe 'D' by the new dataframe
#' call.object <- call("append2df.ds", quote(D), quote(lab_hdl.c), quote(as.character("lab_hdl.c")))
#' datashield.assign(opals, "Dnew", call.object)
#' datashield.assign(opals, "D", quote(Dnew))
#' }
#'
append2df.ds <- function (dataset, column, colname) {
  
  if(is.data.frame(dataset)){
    new.dataset <- data.frame(dataset, column)
    colnames(new.dataset)[length(new.dataset)] <- colname
    return(new.dataset)
  }else{
    stop("\n\''xvect'dataset' muxt be a dataframe!\n\n")
  }
}
