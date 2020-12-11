
context("removeDS::smk::failure")
test_that("failure",
{
   expect_equal(removeDS(),FALSE)
})


context("removeDS::smk::success")
test_that("success",
{
  assign("pie_to_eat",3.14,envir=globalenv())
  expect_equal(existsDS("pie_to_eat", ".GlobalEnv", "numeric"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv","numeric"),TRUE)
})

