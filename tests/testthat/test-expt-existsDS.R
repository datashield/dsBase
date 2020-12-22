context("existDS::expt::param_incorrect")


envir.list <- search()



test_that("incorrenct  param",
{
  expect_equal(existsDS(1,1,1), FALSE)
  expect_equal(existsDS("text",1,1), FALSE)
  expect_equal(existsDS("text","text",1), FALSE)
  expect_equal(existsDS("text","bubba","character"),FALSE)
  expect_equal(existsDS("sink", "package:base","numeric"), FALSE)
}
)


context("existDS::expt::param_correct")
test_that("character_param",
{
  envir.length <- length(envir.list)-1
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
          class.type <- as.character(class(variable))
          expect_equal(existsDS(variable.name, envir.name,class.type), TRUE)
        }

     }
  }
}
)


