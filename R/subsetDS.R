#' 
#' @title Generates a valid subset of a table or a vector
#' @description The function uses the R classical subsetting with squared brackets '[]'
#' @details The user specifies the rows and columns to include in the subset if the input 
#' object is a table and the indices of the entries to include in the subset if the input
#' object is a vector. the name of a vector (i.e. a variable) can also be provided with a logical
#' operator and a threshold. In all cases if the subset is not valid (i.e. contains less than the allowed
#' number of observations), the subset is not generated.
#' @param data a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param criteria a string character, the selection criteria in squared brackets
#' @return a list which contains the subset object
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' library(opal)
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: get a table with the first 500 observations on the first two columns of table D
#' datashield.assign(opals, "Subset", quote(subsetDS("D[1:500,1:2]")))
#' 
#' #' # Example 2: extract the first 500 values of the variable 'LAB_HDL' in the table 'D'
#' datashield.assign(opals, "Subset", quote(subsetDS("D$LAB_HDL[1:500]")))
#' 
#' }
#' 
subsetDS <- function(data=NULL, criteria=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  # check if the input object is defined
  if(!(exists(data))){
    output <- list("The input data you provided is not defined"=NULL)
  }else{
    
    # evaluate the input data object
    D <- eval(parse(text=data))
    
    # check the form of the criteria if there there is a logical operator 
    # split, get variable name and form the expression to evaluate
    rgex <- c(">",">=","<","<=","==","!=")
    tracker <- 0
    for(i in 1:6){
       if(length(grep(rgex[i], criteria)) > 0){
         tracker <- i
       }      
    }
    if(tracker > 0){
      threshold <- strsplit(strsplit(criteria,rgex[tracker])[[1]][2], "\\]")[[1]][1]
      if(is.vector(D) | is.factor(D)){
        exprs <- paste0(data, "[which(", data, rgex[tracker], threshold, ")]")
      }else{
        varname <- strsplit(strsplit(criteria,rgex[tracker])[[1]][1], "\\[")[[1]][2]
        idx <- which(colnames(D) == varname)
        exprs <- paste0(data, "[which(", data, "[,", idx, "]",rgex[tracker], threshold, "),]")
      }
    }else{
      exprs <- paste0(data, criteria)
    }
    
    # subsetting 
    subvect <- eval(parse(text=exprs))
    if(is.vector(subvect) | is.factor(subvect)){
      if(length(subvect) < nfilter){
        output <- list("Invalid subset vector"=NULL)
      }else{
        output <- list(newVector=subvect)
      }
    }else{
      if(dim(subvect)[1] < nfilter){
        output <- list("Invalid subset table"=NULL)
      }else{
        output <- list(newTable=subvect)
      }
    }
  }
  return(output)
}
