

whereDS <- function(variable.name = NULL, envir = caller_env(), class.type = NULL)
{
  if (identical(envir, empty_env()))
  {
    # Base case
    return(NULL)
  }
  else if (exists(variable.name,envir))
  {
    # Success case
    variable.found <- get(variable.name, pos = position)
    if(class(variable.found) == class.type)
    {
      return(envir)
    }
    else
    {
      where(variable.name, env_parent(envir))
    }
  }
  else
  {
    # Recursive case
    where(variable.name, env_parent(envir))
  }
}
