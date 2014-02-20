#' 
#' @title Generates valid subset(s) of a dataset based on the subclasses.
#' @description The function takes a dataframe and generates subset(s)
#' dataframes for all (default) or specified factor variables if the subsets are valid. 
#' By default a subset of the dataframe is created for each categorie
#' of each factor. It is possible to indicate the variable for which
#' a subset is sought by indicating their name. If the variable
#' indicated are not factors no subsets are generated.
#' @param dataset a string character, the name of the dataset.
#' @param variables a vector of string characters, the names of the
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
#' # Example 1: get the subsets for all categories from the table assigned above 
#' # (by default the name of the the assigned dataframe is 'D')
#' datashield.assign(opals, "Subsets", quote(subclass.ds("D")))
#' 
#' #' # Example 2: get the subsets by gender class from the table assigned above 
#' # by default the name of the the assigned dataframe is 'D')
#' datashield.assign(opals, "Subsets", quote(subclass.ds("D", c("GENDER"))))
#' }
#' 
subclass.ds <- function(dataset=NULL, variables=NULL){
  
  # evaluate the string passed on to the function as an object
  D <- eval(parse(text=dataset))
  
  # get the names of the variables on the dataset
  varnames <- colnames(D)
  
  # set the number of loops depending on the number of variables specified
  # if no variable was specified then loop through all the variables
  if(is.null(variables)){
    loop <- c(1:dim(D)[2])
  }else{
    # if the user specified variables to subset on check if those are in the dataset to subset from,
    # if none of the variables is on the dataset record a message to inform the user
    indx1 <- which(varnames %in% variables)
    
    if(length(indx1) > 1){
      loop <- indx1
    }else{
      loop <- 1
    }
  }
  
  # subsetting is carried out only of the table is of type data frame
  if(is.data.frame(D)){
    
    # loop through the variables and make a subset dataframe for each level
    # of each factor variable and keep the generated subset dataframes in a list
    subsets <- list()
    names.of.subsets <- c()
    count <- 0
    nonfactorvars <- 0
    if(length(loop) > 1){
      for(i in loop){
        var <- D[,i]
        varname <- varnames[i]
        if(is.factor(var)){        
          # get the levels
          categories <- levels(var)
          # loop through the levels and generate a dataset for each level
          # if the number of observations for that level > 0 and < 5
          for(j in 1:length(categories)){
            indices <- which(var == as.numeric(categories[j]))
            if(!(length(indices) > 0 & length(indices) < 100)){
              count <- count+1
              subD <- D[indices,]
              subsets[[count]] <- subD
              name.of.subD <- paste(varname,".level_", categories[j], sep="")
              names.of.subsets <- append(names.of.subsets, name.of.subD)
            }else{
              # if any one category has between 1 and 4 observation record a message
              count <- count+1
              subsets[[count]] <- "Invalid category: it has between 0 and 4 observations"
              name.of.subD <- paste(varname,".level_", categories[j], "-INVALID",sep="")
              names.of.subsets <- append(names.of.subsets, name.of.subD)             
            }
          }
        }else{
          # if a variable is not a factor increment the below counter
          nonfactorvars <- nonfactorvars + 1
        }
      }
    }else{
      var <- D[,indx1]
      varname <- variables[indx1]
      if(is.factor(var)){
        # get the levels
        categories <- levels(var)
        # loop through the levels and generate a dataset for each level
        # if the number of observations for that level > 0 and < 5
        for(j in 1:length(categories)){
          indices <- which(var == as.numeric(categories[j]))
          if(!(length(indices) > 0 & length(indices) < 100)){
            count <- count+1
            subD <- D[indices,]
            subsets[[count]] <- subD
            name.of.subD <- paste(varname,".level_", categories[j], sep="")
            names.of.subsets <- append(names.of.subsets, name.of.subD)
          }else{
            # if any one category has between 1 and 4 observation record a message
            count <- count+1
            subsets[[count]] <-"Invalid category: it has between 0 and 4 observations"
            name.of.subD <- paste(varname,".level_", categories[j], "-INVALID",sep="")
            names.of.subsets <- append(names.of.subsets, name.of.subD)         
          }
        }
      }else{
        # if a variable is not a factor increment the below counter
        nonfactorvars <- nonfactorvars + 1
      }
    }
    # if non of variable is a factor inform user
    if(nonfactorvars == length(loop)){
      stop("There are no factor variables to subset on, in the specified dataset!")
    }else{
      # now set the names of subsets in the holder list to the names
      # that were generated along with the subsets
      names(subsets) <- names.of.subsets
      output <- subsets
    }
  }else{
    stop("The dataset to generate the subsets from is not of type dataframe!")
  }
  
  return(output)
}
