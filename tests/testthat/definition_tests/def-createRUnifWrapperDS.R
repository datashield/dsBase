
.test_success_no_param  <- function()
{
  outcome        <- createMatrixRUnifWrapperDS()
  list.variables <- ls(envir=globalenv())
  expect_equal(outcome, TRUE)
  expect_equal("newMatrixRUnif" %in% list.variables, outcome)

  new.matrix     <- get("newMatrixRUnif",globalenv())
  expect_equal(nrow(new.matrix), 11)
  expect_equal(ncol(new.matrix), 13)
  expect_equal(identical(new.matrix[new.matrix[,] < -10^16],numeric(0)), TRUE)
  expect_equal(identical(new.matrix[new.matrix[,] > 10^16],numeric(0)), TRUE)
}


.test_success_size_parameter <- function(no.rows, no.columns)
{
  outcome        <- createMatrixRUnifWrapperDS(no.rows,no.columns)
  list.variables <- ls(envir=globalenv())
  expect_equal(outcome, TRUE)
  expect_equal("newMatrixRUnif" %in% list.variables, outcome)

  new.matrix     <- get("newMatrixRUnif",globalenv())
  expect_equal(nrow(new.matrix), no.rows)
  expect_equal(ncol(new.matrix), no.columns)
  expect_equal(identical(new.matrix[new.matrix[,] < -10^16],numeric(0)), TRUE)
  expect_equal(identical(new.matrix[new.matrix[,] > 10^16],numeric(0)), TRUE)
}

.test_success_all_parameters <- function(no.rows, no.columns,variable.name)
{
  outcome        <- createMatrixRUnifWrapperDS(no.rows,no.columns,variable.name,"globalenv")
  list.variables <- ls(envir=globalenv())
  new.matrix     <- get(variable.name,globalenv())

  expect_equal(outcome, TRUE)
  expect_equal(variable.name %in% list.variables, outcome)
  expect_equal(nrow(new.matrix), no.rows)
  expect_equal(ncol(new.matrix), no.columns)
  expect_equal(identical(new.matrix[new.matrix[,] < -10^16],numeric(0)), TRUE)
  expect_equal(identical(new.matrix[new.matrix[,] > 10^16],numeric(0)), TRUE)
}

.test_success_other_environment <- function()
{
  outcome        <- createMatrixRUnifWrapperDS(11,13,"new.matrix.999","sub_env")
  list.variables <- ls(envir=ds.test_env$sub_env)
  new.matrix     <- get(variable.name,globalenv())

  expect_equal(outcome, TRUE)
  expect_equal(variable.name %in% list.variables, outcome)
  expect_equal(nrow(new.matrix), no.rows)
  expect_equal(ncol(new.matrix), no.columns)
  expect_equal(identical(new.matrix[new.matrix[,] < -10^16],numeric(0)), TRUE)
  expect_equal(identical(new.matrix[new.matrix[,] > 10^16],numeric(0)), TRUE)
}

.test_failure_size_parameter <- function(no.rows, no.columns)
{
  outcome        <- createMatrixRUnifWrapperDS(no.rows,no.columns)
  list.variables <- ls(envir=globalenv())
  expect_equal(outcome,  FALSE)
  expect_equal("newMatrixRUnif" %in% list.variables, outcome)
}

.test_failure_all_parameters <- function(no.rows, no.columns,variable.name, environment)
{
  outcome        <- createMatrixRUnifWrapperDS(no.rows,no.columns,variable.name, environment)
  list.variables <- ls(envir=environment)
  expect_equal(outcome, FALSE)
  expect_equal(variable.name %in% list.variables, outcome)
}
