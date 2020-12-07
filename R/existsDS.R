#'@name existsDS
#'@title Is an object exists defined with an expected internal type?
#'@description This server function indicates whether a variable or R object exists in a specific environment. It also verifies
#'the class of the object correspond to an expected class.
#'@param variable.name A character value that state the variable (R object) to check
#'@param environment.name A character value stating the name of an environment
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{logical}}"
#'\item "\code{\link{numeric}}"
#'\item "\code{\link{single}}"
#'\item "\code{\link{raw}}"
#'\item "\code{\link{vector}}"
#'\item  "\code{\link{S4}}"
#'\item "\code{\link{NULL}}"
#'\item "\code{\link{function}}"
#'\item "\code{\link{externalptr}}"
#'\item "\code{\link{environment}}"
#'}
#'@return
#'\itemize{
#' \item \code{TRUE} if an object is found in a specific environment and it is of a specific type
#' \item \code{FALSE}, if the object does not exists in an environment or it is not made an expected class
#'}
#'@details This function uses the \code{\link{new.env}} R function to create an  environment. The default environment should is set to ".GlobalEnv".
#'However, any environments available on the search path can be specified; use \code{\link{search()}} function to identify them.
#'
#'The function \code{\link{class}} verifies the R object has an expected class.
#'
#'Type: aggregate server function
#'@seealso
#'\code{\link{exists}}, \code{\link{typeof}}, \code{\link{class}}, \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/BasicClasses.html}
#'@author P.Ryser-Welch, Newcastle University, DataSHIELD team
#

existsDS <- function(variable.name = NULL, environment.name = ".GlobalEnv", class.type = NULL)
{

  #All the characters have the correct types
  exists <- FALSE
  if(is.character(variable.name) && is.character(environment.name) && is.character(class.type))
  {

    #verifies the environment exists
    list.environments <- search()

    #get the position of the environment in the environments list
    position <- which(list.environments %in% environment.name)
    if(!identical(position,integer(0)))
    {

       #check variable exists in environment
       if (exists(variable.name, where = position))
       {
         variable.found <- get(variable.name, pos = position)
         if(class(variable.found) == class.type)
         {
            exists <- TRUE
         }
       }
    }

  }
  return(exists)
}

