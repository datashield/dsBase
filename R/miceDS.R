#'
#' @title Aggregate function called by ds.mice
#' @description This function is a wrapper function of the mice from the mice R 
#' package. The function creates multiple imputations (replacement values)
#' for multivariate missing data. The method is based on Fully Conditional Specification, 
#' where each incomplete variable is imputed by a separate model. The MICE algorithm can
#' impute mixes of continuous, binary, unordered categorical and ordered categorical data.
#' In addition, MICE can impute continuous two-level data, and maintain consistency between
#' imputations by means of passive imputation.
#' @details For additional details see the help header of mice function in native R mice
#' package.
#' @param data a data frame or a matrix containing the incomplete data. 
#' @param m Number of multiple imputations. The default is m=5. The maximum allowed 
#' number in DataSHIELD is m=20.
#' @param maxit A scalar giving the number of iterations. The default is 5. The maximum 
#' allowed number in DataSHIELD is maxit=30.
#' @param method Can be either a single string, or a vector of strings with length 
#' ncol(data), specifying the imputation method to be used for each column in data. If 
#' specified as a single string, the same method will be used for all blocks. The default 
#' imputation method (when no argument is specified) depends on the measurement level of 
#' the target column, as regulated by the defaultMethod argument in native R mice function. 
#' Columns that need not be imputed have the empty method "".
#' @param post A vector of strings with length ncol(data) specifying expressions as strings. 
#' Each string is parsed and executed within the sampler() function to post-process imputed 
#' values during the iterations. The default is a vector of empty strings, indicating no 
#' post-processing. Multivariate (block) imputation methods ignore the post parameter.
#' @param seed either NA (default) or "fixed". If seed is set to "fixed" then a fixed
#' seed random number generator which is study-specific is used. 
#' @param predictorMatrix A numeric matrix of ncol(data) rows and ncol(data) columns, 
#' containing 0/1 data specifying the set of predictors to be used for each target column.
#' Each row corresponds to a variable to be imputed. A value of 1 means that the column 
#' variable is used as a predictor for the target variables (in the rows). By default, the
#' predictorMatrix is a square matrix of ncol(data) rows and columns with all 1's, except 
#' for the diagonal.
#' @param ncol.pred.mat the number of columns of the predictorMatrix.
#' @param newobj_mids a character string that provides the name for the output mids object
#' that is stored on the data servers. Default \code{mids_object}. 
#' @param newobj_df a character string that provides the name for the output dataframes 
#' that are stored on the data servers. Default \code{imputationSet}. For example, if m=5, and 
#' newobj_df="imputationSet", then five imputed dataframes are saved on the servers with names
#' imputationSet.1, imputationSet.2, imputationSet.3, imputationSet.4, imputationSet.5.
#' @return a list with three elements: the method, the predictorMatrix and the post. 
#' The function also saves in each server the mids object and all completed datasets as 
#' dataframes.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @import mice 
#' @export
#'
miceDS <- function(data=data, m=m, maxit=maxit, method=method, post=post, seed=seed,
                   predictorMatrix=predictorMatrix, ncol.pred.mat=ncol.pred.mat, 
                   newobj_mids=newobj_mids, newobj_df=newobj_df){
  
  if(seed %in% c(NA, 'NA')){
    seed <- NA
  }else{  
    # the study-specific seed for random number generation
    seed <- getOption("datashield.seed")
  }
  
  data <- eval(parse(text=data), envir = parent.frame())
  
  if(!is.null(method)){
    method <- unlist(stringr::str_split(method, pattern=","))
  }
  
  if(!is.null(post)){
    post <- gsub("left_parenthesis", "(", post, fixed = TRUE)
    post <- gsub("right_parenthesis", ")", post, fixed = TRUE)
    post <- gsub("left_square_bracket", "[", post, fixed = TRUE)
    post <- gsub("right_square_bracket", "]", post, fixed = TRUE)
    post <- gsub("equalR_symbol", "<-", post, fixed = TRUE)
    post <- gsub("equal_symbol", "=", post, fixed = TRUE)
    post <- gsub("comma_symbol", ",", post, fixed = TRUE)
    post <- unlist(stringr::str_split(post, pattern="separ_comma"))
  }
  
  if(!is.null(ncol.pred.mat)){
    predictorMatrix <- as.numeric(unlist(strsplit(predictorMatrix, split=",")))
    if(!all(predictorMatrix %in% 0:1)){
      stop("The predictorMatrix should contains only 0/1 values", call. = FALSE)
    }
    predictorMatrix <- matrix(predictorMatrix, ncol=ncol.pred.mat)
  }else{
    # create a square matrix of ncol(data) rows and columns with all 1's, except for the diagonal.
    predictorMatrix <- matrix(1, ncol=ncol(data), nrow=ncol(data))
    diag(predictorMatrix) <- 0
  }

  # call the mice function to do the imputation
  imputed <- mice(data=data, m=m, maxit=maxit, method=method, post=post, seed=seed,
              predictorMatrix=predictorMatrix)
  
  # save the imputed object
  base::assign(newobj_mids, imputed, envir = parent.frame())

  # save the imputed datasets as data.frames on the server-side
  for (i in 1:m){
    base::assign(paste0(newobj_df,'.',i), complete(imputed, i), envir = parent.frame())
  }
  
  output <- list(method=imputed$method, predictorMatrix=imputed$predictorMatrix, 
                 post=imputed$post)
 
  return(output) 
  
}  