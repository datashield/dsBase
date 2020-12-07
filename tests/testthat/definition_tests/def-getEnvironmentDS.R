


.test_no_parameter <- function()
{
  empty_env = getEnvironmentDS()
  expect_equal(empty_env,emptyenv())
  input <- ls(emptyenv())
  output <- ls(empty_env)
  expect_equal(identical(input, character(0)),TRUE)
  expect_equal(identical(output, character(0)),TRUE)
}


.test_incorrect_parameter <- function(environment.name, package.name)
{
  empty_env = getEnvironmentDS(environment.name,package.name)
  expect_equal(empty_env,emptyenv())
  input <- ls(emptyenv())
  output <- ls(empty_env)
  expect_equal(identical(input, character(0)),TRUE)
  expect_equal(identical(output, character(0)),TRUE)
}


.test_correct_parameters <- function()
{
  envir.list <- search()
  envir.length <- length(envir.list)
  for (position in 2:envir.length)
  {
    envir.name <- envir.list[position]
    var.list   <- ls(pos = position)
    var.length <- length(var.list)
    for (var.index in 1:var.length)
    {
      variable.name <- var.list[var.index]
      if (!is.na(variable.name) && length(variable.name) > 0)
      {
        variable   <- get(variable.name, envir.name)
        output.env <- getEnvironmentDS(environment.name = variable.name, package.name = envir.name)
        if(is.environment(variable))
        {
          #expect_equal(output.env,variable)
          input <- ls(variable)
          output <- ls(output.env)
          expect_equal(all(input == output),TRUE)
        }
        else
        {

          expect_equal(output.env,emptyenv())
          output <- ls(output.env)
          expect_equal(identical(output, character(0)),TRUE)
        }
      }
    }
  }
}
