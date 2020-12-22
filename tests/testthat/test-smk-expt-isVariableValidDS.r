context("isNumVariableValidDS::smk")
test_that("no variable",
{
  expect_false(isNumVariableValidDS("D$Integer"))
})


local.data   <- read.csv("data_files/DATASET1.csv")

local.tibble <- tibble('INTEGER'   = local.data$INTEGER,
                       'CHARACTER' = local.data$CHARACTER,
                       'LOGICAL'   = local.data$LOGICAL,
                       'NUMERIC'   = as.numeric(local.data$NUMERIC))
assign("pi_value", pi, pos = 1)
assign("D", local.data, pos = 1)
assign("E", NULL, pos = 1)
assign("F", local.tibble, pos = 1)
assign("G", matrix(c("a","b","c","d"),2,2), pos = 1)

test_that("incorrect variable",
{
  expect_false(isNumVariableValidDS("D$Integers"))
})

test_that("correct variable",
{
  expect_false(isNumVariableValidDS("pi_value"))
  expect_true(isNumVariableValidDS("D$INTEGER"))
  expect_true(isNumVariableValidDS("D$NUMERIC"))
  expect_true(isNumVariableValidDS("F$INTEGER"))
  expect_true(isNumVariableValidDS("F$NUMERIC"))
})

context("isNumVariableValidDS::expt")
test_that("incorrect param",
{
  expect_false(isNumVariableValidDS())
  expect_false(isNumVariableValidDS(x = 1))
})

test_that("incorrect data type",
{
  expect_false(isNumVariableValidDS(x = "D$CHARACTER"))
  expect_false(isNumVariableValidDS(x = "D$BOOLEAN"))
  expect_false(isNumVariableValidDS(x = "D$LOGICAL"))
  expect_false(isNumVariableValidDS(x = "E"))
  expect_false(isNumVariableValidDS(x = "F$CHARACTER"))
  expect_false(isNumVariableValidDS(x = "F$BOOLEAN"))
  expect_false(isNumVariableValidDS(x = "F$LOGICAL"))
  expect_false(isNumVariableValidDS(x = "G"))
})

