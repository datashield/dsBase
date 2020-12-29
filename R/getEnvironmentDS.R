#'@name getEnvironmentDS
#'@title retrieves an environment from its name.
#'@description retrieves an environment from its name. It verifies the environment exists within the specified parent environment, before returning the environment itself.
#'@details The helper function verifies a environment exists on the search path.By default, the parent environment used is the ".GlobalEnv".
#'@param environment.name the name of an environment available on a DataSHIELD server.
#'@param package.name the name of the package as appearing in \code{\link{search}}
#'@return
#'\itemize{
#' \item  an empty enviroment if no environment is found.
#' \item  the reference to the environment itself.
#'}
#'@details The helper function verifies a environment exists, within another environment  on a dataSHIELD server. By defautl the parent environment used is the
#'\code{globalenv()}. For testing and other purposes, other parent environment can be specify
#'@keywords
#'@author P.Ryser-Welch, Newcastle University, DataSHIELD team
#

getEnvironmentDS <- function(environment.name = NULL, package.name = ".GlobalEnv")
{
  outcome = emptyenv()
  if(is.character(environment.name) && is.character(package.name))
  {
    environment.exists <- existsDS(variable.name = environment.name,
                          environment.name = package.name,
                          class.type = "environment")

    if(environment.exists)
    {
       envir <-  get(environment.name,package.name)

       if (is.environment(envir))
       {
         outcome <- envir
       }
    }
  }
  return(outcome)
}
#helper internal function
