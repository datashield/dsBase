#' 
#' @title Breaks down a dataframe or a factor into its sub-classes
#' @description The function takes a categorical vector or dataframe as input and generates subset(s)
#' vectors or dataframes for each category. Subsets are considered invalid if they hold between 1 and 
#' 4 observations.
#' @details If the input data object is a dataframe it is possible to specify  the variables  
#' to subset on. If a subset is not 'valid' all its the values are reported as missing (i.e. NA),
#' the name of the subsets is labelled as '_INVALID'. If no variables are specified to subset on, 
#' the dataframe will be subset on each of its factor variables.
#' And if none of the columns holds a factor variable a message is issued as output. A message is also
#' issued as output if the input vector is not of type factor.
#' @param data a string character, the name of the dataframe or the factor vector
#' @param variables a vector of string characters, the names of the the variables to subset on.
#' @return a list which contains the subsetted datasets 
#' @author Gaye, A.
#' @export
#' 
subsetByClassDS <- function(data=NULL, variables=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  # evaluate the string passed on to the function as an object
  input <- eval(parse(text=data), envir = parent.frame())
  
  # subsetting is carried out only of the input is of type factor or data.frame
  if(is.factor(input)){
    # call the internal function that generates subsets if the input is a factor variable
    Dname <- extract(data)[[2]]
    output <- subsetByClassHelper1(input, Dname, nfilter)
  }else{
    # get the names of the variables on the dataset
    varnames <- colnames(input)
    
    # set the number of loops depending on the number of variables specified
    # if no variable was specified then loop through all the variables
    if(is.null(variables)){
      loop <- c(1:dim(input)[2])
    }else{
      # if the user specified variables to subset on check if those are in the dataset to subset from,
      # if none of the variables is on the dataset record a message to inform the user
      indx <- which(varnames %in% variables)
      
      if(length(indx) > 1){
        loop <- indx
      }else{
        loop <- 1
      }
    }
    
    # loop through the variables and make a subset dataframe for each level
    # of each factor variable and keep the generated subset dataframes in a list
    if(length(loop) > 1){
      # call the function that gets the subsets if the user specified non or more than 1 variable
      out.temp <- subsetByClassHelper2(input,loop,nfilter)
      subsets <- out.temp[[1]]
      nonfactorvars <- out.temp[[2]]
    }else{
      # call the function that gets the subsets if the user specified only one variable to subset by
      out.temp <- subsetByClassHelper3(input,indx,nfilter)
      subsets <- out.temp[[1]]
      nonfactorvars <- out.temp[[2]]
    }
    output <- subsets
  }
  
  return(output)
}
