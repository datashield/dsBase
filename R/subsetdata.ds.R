#' 
#' @title Generates a valid subset of a dataset
#' @details The function takes a dataframe and generates a subset
#' dataframes for all the factor variables if that subset is valid. 
#' By default a subset of the dataframe is created for each categorie
#' of each factor. It is possible to indicate the variable for which
#' a subset is sought by indicating their columns indices. The variable
#' at the given indices are not factors no subsets are generated.
#' @param dataset a dataframe
#' @param columns a a numeric vector that gives the subsets of the
#' variables for which subsets are sougth.
#' @return a list which contains the subsetted datasets 
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: get the subsets from the table assigned above (by default is 'D')
#' datashield.assign(opals, "Subsets", quote(subsetdata.ds(D)))
#' 
#' #' # Example 2: get the subsets from the table assigned above (by default is 'D')
#' datashield.assign(opals, "Subsets", quote(subsetdata.ds(D, list(1,4))))
#' }
#' 
subsetdata.ds <- function(dataset=NULL, columns=NULL){
  
  if(is.null(dataset)){
    stop("\n\nNo dataset provided!\n\n")
  }
  
  if(is.null(columns)){
    # the names of the variables in the dataset
    D <- dataset
    variables <- colnames(D)
  }else{
    # the names of the variables in the dataset
    indices <- unlist(columns)
    D <- dataset[,indices]
    colnames(D) <- colnames(dataset)[indices]
    variables <- colnames(D)
  }
  
  if(is.data.frame(dataset)){
        
    # the names of the variables in the dataset
    D <- dataset
    variables <- colnames(D)
    
    # the number of variables in the dataset
    numvar <- length(variables)
    
    # loop through the variables and make a subset dataframe for each level
    # of each factor variable and keep the generated subset dataframes in a list
    subsets <- list()
    names.of.subsets <- c()
    count <- 0
    for(i in 1:numvar){
      var <- D[,i]
      varname <- variables[i]
      if(is.factor(var)){
        # get the levels
        categories <- levels(var)
        # loop through the levels and generate a dataset for each level
        # if the number of observations for that level > 0 and < 5
        for(j in 1:length(categories)){
          indices <- which(var == as.numeric(categories[j]))
          if(!(length(indices) > 0 & length(indices) < 5)){
            count <- count+1
            subD <- D[indices,]
            subsets[[count]] <- subD
            name.of.subD <- paste(varname,".level_", categories[j], sep="")
            names.of.subsets <- append(names.of.subsets, name.of.subD)
          }
        }
      }
    }
    # now set the names of subsets in the holder list to the names
    # that were generated along with the subsets
    names(subsets) <- names.of.subsets
    return(subsets)
    
  }else{
    stop("\n\nThe dataset is not a dataframe!\n\n")
  }

}
