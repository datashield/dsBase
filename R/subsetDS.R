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
#' @param complt a boolean that tells if the subset to subset should include only complete cases
#' @param rs a vector of two integers that give the range of rows de extract. 
#' @param cs a vector of two integers or one or more characters; the indices of the columns to extract or the names of the columns (i.e. 
#' names of the variables to extract).
#' @param lg a character, the logical parameter to use if the user wishes to subset a vector using a logical 
#' operator. This parameter is ignored if the input data is not a vector.
#' @param th a numeric, the threshold to use in conjunction with the logical parameter. This parameter is ignored 
#' if the input data is not a vector.
#' @param varname a character, if the input data is a table, if this parameter is provided along with the 'logical' and 'threshold'
#' parameters, a subtable is based the threshold applied to the speicified variable. This parameter is however ignored if the parameter
#' 'rows' and/or 'cols' are provided.
#' @return a subset of the vector, matric or dataframe as specified is stored on the server side
#' @author Gaye, A.
#' @export
#' 
subsetDS <- function(dt=NULL, complt=NULL, rs=NULL, cs=NULL, lg=NULL, th=NULL, varname=NULL){
  
  # this filter sets the minimum number of observations that are allowed 

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

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
  D <- eval(parse(text=dt), envir = parent.frame())
  
  # if 'complt' is set to TRUE, get continue with a dataset with complete cases only
  if(complt){
    cc <- stats::complete.cases(D)
    xx <- which(cc == TRUE)
    Dtemp <- D
    if(is.vector(D) | is.factor(D)){
      D <- Dtemp[xx]
    }else{
      D <- Dtemp[xx,]
    }
  }

  # carry out the subsetting
  if(is.vector(D) | is.factor(D)){ # if the input data is a vector
    
    if(is.null(rs)){
      if(is.null(lg) | is.null(th)){
        subvect <- D
      }else{
        exprs1 <- paste0("D[which(D", lg, th, ")]")
        subvect <- eval(parse(text=exprs1))
      }
    }else{
      subvect <- D[rs]
    }

    if(length(subvect) < nfilter.tab){
      if(length(subvect) == 0){
        output <- D[-c(1:length(D))]
      }else{
        temp1 <- subvect
        temp1[1:length(temp1)] <- NA
        output <- temp1
      }
    }else{
      output <- subvect
    }
  }else{ # if the input data is a table
    
    if(!(is.null(rs)) | !(is.null(cs))){
      if(!(is.null(rs)) & !(is.null(cs))){
        subtable <- D[rs, cs]
      }else{
        if(is.null(cs)){
          cs <- c(1:dim(D)[2])
        }
        if(is.null(rs)){
          rs <- c(1:dim(D)[1])
        }
        subtable <- D[rs,cs]
      }
    }else{
      if(is.null(varname)){
        subtable <- D
      }else{
        idx <- which(colnames(D) == varname)
        exprs2 <- paste0('D[which(D[,',idx,']', lg, th, '),]')
        subtable <- eval(parse(text=exprs2))
      }
    }
    
    if((dim(subtable)[1]) < nfilter.tab){
      if((dim(subtable)[1]) == 0){
        output <- D[-c(1:dim(D)[1]),]
      }else{
        subD <- subtable
        subD[] <- NA
        output <- subD
      }
    }else{
      output <- subtable
    }
  }
  
  return(output)
}
