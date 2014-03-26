#' 
#' @title Generates a valid subset of a table or a vector
#' @description The function uses the R classical subsetting with squared brackets '[]' and allows also to 
#' subset using a logical oprator and a threshold. The object to subset from must be a vector (factor, numeric 
#' or charcater) or a table (data.frame or matrix).
#' @details If the input data is a table: The user specifies the rows and/or columns to include in the subset if the input 
#' object is a table; the columns can be refered to by their names. The name of a vector (i.e. a variable) can also be provided 
#' with a logical operator and a threshold (see example 3). 
#' If the input data is a vector:  when the parameters 'rows', 'logical' and 'threshold' are all provided the last two are ignored (
#' 'rows' has precedence over the other two parameters then).
#' If the requested subset is not valid (i.e. contains less than the allowed
#' number of observations), the subset is not generated, rather a table or a vector of missing values is generated to allow
#' for any subsequent process using the output of the function to proceed after informing the user via a message.
#' @param dt a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param rs a vector of integers, the indices of the rows de extract. 
#' @param cs a vector of integers or characters; the indices of the columns to extract or the names of the columns (i.e. 
#' names of the variables to extract).
#' @param lg a charcater, the logical parameter to use if the user wishes to subset a vector using a logical 
#' operator. This parameter is ignored if the input data is not a vector.
#' @param th a numeric, the threshold to use in conjunction with the logical parameter. This parameter is ignored 
#' if the input data is not a vector.
#' @param var a character, if the input data is a table, if this parameter is provided along with the 'logical' and 'threshold'
#' parameters, a subtable is based the threshold applied to the speicified variable. This parameter is however ignored if the parameter
#' 'rows' and/or 'cols' are provided.
#' @return a no data are return to the user but messages are printed out.
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
#' datashield.assign(opals, "subD", quote(subsetDS(dt='D', rs=c(1:500), cs=c(1,2))))
#' 
#' #' # Example 2: subset 'D' on bmi values greater than 25
#' datashield.assign(opals, "subD", quote(subsetDS(dt='D', lg=1, th=25)))
#' 
#' }
#' 
subsetDS <- function(dt=NULL, rs=NULL, cs=NULL, lg=NULL, th=NULL, var=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  # the logical operators are given as integers change them into characters
  if(!(is.null(lg))){
    if(lg == 1){lg <- ">"}
    if(lg == 2){lg <- ">="}
    if(lg == 3){lg <- "<"}
    if(lg == 4){lg <- "<="}
    if(lg == 5){lg <- "=="}
    if(lg == 6){lg <- "!="}
  }
  
  # evaluate the input data object
  D <- eval(parse(text=dt))
  
  # carry out the subsetting
  if(is.vector(D) | is.factor(D)){ # if the input data is a vector
    if(!(is.null(rs))){
      subvect <- D[rs]
    }else{
      exprs1 <- paste0(dt, "[which(", dt, lg, th, ")]")
      subvect <- eval(parse(text=exprs1))
    }
    
    if(length(subvect) < nfilter){
      emptyVect <- rep(NA, length(subvect))
      output <- list(emptyVect)
      names(output) <- paste0("new_", dt, "_INVALID")
    }else{
      output <- list(subvect)
      names(output) <- paste0("new_", dt)
    }
  }else{ # if the input data is a table
    if(!(is.null(rs)) | !(is.null(cs))){
      if(!(is.null(rs)) & !(is.null(cs))){
        subtable <- D[rs,cs]
      }else{
        if(!(is.null(rs)) & is.null(cs)){
          subtable <- D[rs,]
        }else{
          subtable <- D[,cs]
        }
      }
    }else{
      indx <- which(colnames(D) == var)
      exprs2 <- paste0(dt, "[which(", dt, "[,",indx,"]", lg, th, "),]")
      subtable <- eval(parse(text=exprs2))
    }
    
    if(dim(subtable)[1] < nfilter){
      emptyTable <- data.frame(matrix(NA, nrow=dim(subtable)[1], ncol=dim(subtable)[2]))
      colnames(emptyTable) <- colnames(subtable)
      output <- list(emptyTable)
      names(output) <- paste0("new_", dt, "_INVALID")
    }else{
      output <- list(subtable)
      names(output) <- paste0("new_",dt)
    }
  }

  return(output)
}
