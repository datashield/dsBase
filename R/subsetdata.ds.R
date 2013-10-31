#' 
#' @title Generates a valid subset of a dataset
#' @details The function takes a dataframe and generates a subset
#' dataframes for all the factor variables if that subset is valid. 
#' By default a subset of the dataframe is created for each categorie
#' of each factor. It is possible to indicate the variable for which
#' a subset is sought by indicating their columns indices. The variable
#' at the given indices are not factors no subsets are generated.
#' @param dataset a string character, the name of the dataset.
#' @param columns a a numeric vector that gives the subsets of the
#' variables for which subsets are sougth.
#' @return a list which contains the subsetted datasets 
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
#' # Example 1: get the subsets from the table assigned above (by default is 'D')
#' datashield.assign(opals, "Subsets", quote(subsetdata.ds("D")))
#' 
#' #' # Example 2: get the subsets from the table assigned above (by default is 'D')
#' datashield.assign(opals, "Subsets", quote(subsetdata.ds("D", list(4))))
#' }
#' 
subsetdata.ds <- function(dataset=NULL, columns=NULL){
  
  # evaluate the object that is passed on to the function as an object
  dataset <- eval(parse(text=dataset))
  
  if(is.null(dataset)){
    stop("\n\nNo dataset provided!\n\n")
  }
  
  if(is.null(columns)){
    loop <- c(1:dim(dataset)[2])
  }else{
    indx <- unlist(columns)
    if(length(indx) > 1){
      loop <- indx
    }else{
      loop <- 1
    }
  }
  
  if(is.data.frame(dataset)){
        
    # the names of the variables in the dataset
    D <- dataset
    variables <- colnames(D)
    
    # loop through the variables and make a subset dataframe for each level
    # of each factor variable and keep the generated subset dataframes in a list
    subsets <- list()
    names.of.subsets <- c()
    count <- 0
    if(length(loop) > 1){
      for(i in loop){
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
            }else{
              # if any one category has between 1 and 4 observation return NULL and stop
              return(NULL)
              stop("\n One of the categories is not valid: it has between 0 and 4 observations!\n")            
            }
          }
        }
      }
    }else{
      var <- D[,indx]
      varname <- variables[indx]
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
          }else{
            # if any one category has between 1 and 4 observation return NULL and stop
            return(NULL)
            stop("\n One of the categories is not valid: it has between 0 and 4 observations!\n")            
          }
        }
      }
      
    }
    # now set the names of subsets in the holder list to the names
    # that were generated along with the subsets
    names(subsets) <- names.of.subsets
    return(subsets)
  }else{
    return(NULL)
    stop("\nThe dataset is not a dataframe!\n")
  }

}
