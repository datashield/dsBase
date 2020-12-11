#'@title removeDS  - destroys an environment on the server
#'@description Remove objects (variables) of a specific data type on a DataSHIEL server
#'@param variable.name A character value that state the variable (R object) to check
#'@param environment.name A character value stating the name of an environment. (1) It could  be the name of the package as appearing in \code{\link{search}}.
#'It could also be the name of child environment created in the global enviromment or an package environment.
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{matrix}}"
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
#'#'@param package.name the name of the package as appearing in \code{\link{search}} (optional)
#'@return
#'\itemize{
#'\item TRUE  The environment has been destroyed successfully
#'\item FALSE No environment has been destroyed
#'}
#'@details  This server
#'
#'TYPE: AGGREGATE SERVER FUNCTION
#'@author P.Ryser-Welch, Newcastle University, DataSHIELD team
#

removeDS <- function(variable.name = NULL, environment.name =  "",
                     class.type = NULL, package = sys.frame())
{
  success  <-  FALSE
  if(is.character(variable.name) && is.character(environment.name) && is.character(class.type) && is.environment(package))
  {

    if(length(environment) > 1)
    {
      envir <- getEnvironmentDS(environment.name)
    }
    else
    {
      envir = package
    }

    if (!is.null(envir))
    {
        list.var <- ls(envir = envir)
        if (variable.name %in% list.var)
        {
            variable <- get(variable.name, envir = envir)
            if (class(variable) == class.type)
            {
              rm(list = variable.name, envir = envir)
              still.exists <- existsDS(variable.name, environment.name, class.type)
              success <- identical(still.exists, FALSE)
            }
        }
    }
  }
  return(success)
}


