#' 
#' @title generates subset tables from a data frame
#' @description This is an internal function called by the function 'subsetByClassDS'
#' @details The function generates subsets if the input of 'subsetByClassDS' is a data frame 
#' and if the number of variables (columns) to subset by is 1; i.e. this 
#' function is called if the user specified one variable to subset by.
#' @param df a data frame.
#' @param indx1 the column index of the variable specified by the user.
#' @param filter the minimum number of observations (i.e. rows) that are allowed.
#' @return a list which contains the subsets, their names and an integer that indicates if
#' the variable specified by user is a factor.
#' @keywords internal
#' @noRd
#' @author Gaye, A.
#'
subsetByClassHelper3 <- function(df=NULL, indx1=NULL, filter=NULL){
  # various counters and temporary variables to hold info
  subsets <- list()
  names.of.subsets <- c()
  count <- 0
  nonfactorvars <- 0
  ncols <- length(colnames(df))
  var <- df[,indx1]
  varname <- colnames(df)[indx1]
  if(is.factor(var)){
    # get the levels
    categories <- levels(var)
    # loop through the levels and generate a dataset for each level
    # if the number of observations for that level > 0 and < 'filter'
    for(j in 1:length(categories)){
      indices <- which(var == as.numeric(categories[j]))
      if(!(length(indices) < filter)){
        count <- count+1
        subD <- df[indices,]
        subsets[[count]] <- subD
        name.of.subD <- paste(varname,".level_", categories[j], sep="")
        names.of.subsets <- append(names.of.subsets, name.of.subD)
      }else{
        # if any one category has between 1 and 'filter' number of observations turn subset content into missing values
        count <- count+1
        if(length(indices) == 0){
          subsets[[count]] <- df[-c(1:dim(df)[1]),]
          name.of.subD <- paste(varname,".level_", categories[j], "_EMPTY",sep="")
        }else{
          subD <- df[indices,]
          subD[] <- NA
          subsets[[count]] <- subD
          name.of.subD <- paste(varname,".level_", categories[j], "_INVALID",sep="")
        }
        colnames(subsets[[count]]) <- colnames(df)
        names.of.subsets <- append(names.of.subsets, name.of.subD)
      }
    }
    names(subsets) <- names.of.subsets
  }else{
    nonfactorvars <- 1
  }
  return(list(subsets, nonfactorvars))
}