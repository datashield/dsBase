#' 
#' @title Breaks down a dataframe or a factor into its sub-classes
#' @description The function takes a categorical vector or dataframe as input and generates subset(s)
#' vectors or dataframes for each category. Subsets are considered invalid if they hold between 1 and 
#' 4 observations.
#' @details If the input data object is a dataframe it is possible to specify  the variables  
#' to subset on. If a subset is not 'valid' all its the values are reported as missing (i.e. NA),
#' the name of the subsets is labelled as '_INVALID'. If no variables are specified to subset on, 
#' the dataframe will be subset on each of its factor variables.
#' And if none of the columns holds a factyopr variable a message is issued as output. A message is also
#' issued as output if the input vector is not of type factor.
#' @param data a string character, the name of the dataframe or the factor vector
#' @param variables a vector of string characters, the names of the the variables to subset on.
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
#' datashield.assign(opals, "Subsets", quote(subclassDS("D")))
#' 
#' #' # Example 2: get the subsets by gender class from the table assigned above 
#' # by default the name of the the assigned dataframe is 'D')
#' datashield.assign(opals, "Subsets", quote(subclassDS("D", c("GENDER"))))
#' 
#' #' # Example 3: create a single variable 'gender' and split it into two vectors: males and females
#' datashield.assign(opals, "gender", quote(D$GENDER))
#' datashield.assign(opals, "mf.tables", quote(subclassDS("gender")))
#' }
#' 
subclassDS <- function(data=NULL, variables=NULL){
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  # check if the input object is defined
  if(!(exists(data))){
    output <- list("The input data you provided is not defined"=NULL)
  }else{
    
      # evaluate the string passed on to the function as an object
      D <- eval(parse(text=data))
      
      # subsetting is carried out only of the input is of type factor or data.frame
      if(is.factor(D)){
          # call the internal function that generates subsets if the input is a factor variable
          Dname <- data
          output <- dsbase:::.subclassDShelper1(D, Dname, nfilter)
      }else{
        if(is.data.frame(D)){
          
         # get the names of the variables on the dataset
          varnames <- colnames(D)
          
          # set the number of loops depending on the number of variables specified
          # if no variable was specified then loop through all the variables
          if(is.null(variables)){
            loop <- c(1:dim(D)[2])
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
            out.temp <- dsbase:::.subclassDShelper2(D,loop,nfilter)
            subsets <- out.temp[[1]]
            nonfactorvars <- out.temp[[2]]
          }else{
            # call the function that gets the subsets if the user specified only one variable to subset by
            out.temp <- dsbase:::.subclassDShelper3(D,indx,nfilter)
            subsets <- out.temp[[1]]
            nonfactorvars <- out.temp[[2]]
          }
          # if non of variables
          if(nonfactorvars == length(loop)){
            if(is.null(variables)){
              subsets <- list("The input table holds no factor variables"=NULL)
            }else{
              subsets <- list("The variables to subset by must be factors"=NULL)
            }
            output <- subsets
          }else{
            # now set the names of subsets in the holder list to the names
            # that were generated along with the subsets
            output <- subsets
          }
        }else{
          output <- list("The input data must be a factor or a dataframe"=NULL)
        }
     }
  }
  return(output)
}
