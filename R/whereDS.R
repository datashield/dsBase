

whereDS <- function(variable.name = NULL, envir = rlang::caller_env(), class.type = NULL)
{
  if (identical(envir, rlang::empty_env()))
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
      where(variable.name, rlang::env_parent(envir))
    }
  }
  else
  {
    # Recursive case
    where(variable.name, rlang::env_parent(envir))
  }
}
