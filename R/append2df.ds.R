#'
#' @title Appends a column(s) to a dataframe
#' @param dataset a dataframe
#' @param columns a list of columns/vectors to append to the dataframe
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
#' datashield.assign(opals, "lab_hdl.c", quote(D$LAB_HDL - mean.lab_hdl))
#' 
#' # now append 'lab_hdl.c' to the initially assigned dataframe 'D'
#' # and replace the initial dataframe 'D' by the new dataframe
#' datashield.assign(opals, "D", quote(append2df.ds(D,list(lab_hdl.c))))
#' }
#'
append2df.ds <- function (dataset, columns) {
  
  if(is.data.frame(dataset)){
    new.dataset <- dataset
    for(i in 1:length(columns)){
      new.dataset <- data.frame(new.dataset, columns[[i]])
    }
    return(new.dataset)
  }else{
    stop("\n\''xvect'dataset' muxt be a dataframe!\n\n")
  }
}
