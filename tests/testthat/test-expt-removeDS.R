
context("removeDS::expt::incorrect_variable.name")
test_that("failure",
{
  expect_equal(removeDS(1,environment.name = ".GlobalEnv","numeric"),FALSE)
  expect_equal(removeDS(1.5,environment.name = ".GlobalEnv","numeric"),FALSE)
  expect_equal(removeDS(TRUE,environment.name = ".GlobalEnv","numeric"),FALSE)
  expect_equal(removeDS(FALSE,environment.name = ".GlobalEnv","numeric"),FALSE)
  expect_equal(removeDS(new.env(),environment.name = ".GlobalEnv","numeric"),FALSE)
  expect_equal(removeDS(data.frame(),environment.name = ".GlobalEnv","numeric"),FALSE)
  expect_equal(removeDS(NULL,environment.name = ".GlobalEnv","numeric"),FALSE)

})


context("removeDS::expt::incorrect_environment.name")
test_that("failure",
{
  expect_equal(removeDS("my.var",environment.name = new.env(),"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = TRUE,"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = FALSE,"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = TRUE,"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = 1,"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = 1.5,"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = 1i,"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = globalenv(),"numeric"),FALSE)
  expect_equal(removeDS("my.var",environment.name = "globalenv()","numeric"),FALSE)
})

context("removeDS::expt::incorrect_class.type")
test_that("failure",
{


  assign("pie_to_eat",3.14,envir=globalenv())
  expect_equal(existsDS("pie_to_eat", ".GlobalEnv", "numeric"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",1),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",2.5),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",FALSE),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",TRUE),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",""),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",NULL),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",matrix()),FALSE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv",c()),FALSE)
})

context("removeDS::expt::global_env")
test_that("success",
{

  #null
  assign("null_value",NULL,envir=globalenv())
  expect_equal(existsDS("null_value", ".GlobalEnv", "NULL"),TRUE)
  expect_equal(removeDS("null_value",environment.name = ".GlobalEnv","NULL"),TRUE)
  expect_equal(existsDS("null_value", ".GlobalEnv", "NULL"),FALSE)


   #logical
  assign("sunny_day",TRUE,envir=globalenv())
  expect_equal(existsDS("sunny_day", ".GlobalEnv", "logical"),TRUE)
  expect_equal(removeDS("sunny_day",environment.name = ".GlobalEnv","logical"),TRUE)
  expect_equal(existsDS("sunny_day", ".GlobalEnv", "logical"),FALSE)

  #numeric
  assign("pie_to_eat",as.numeric(3.14),envir=globalenv())
  expect_equal(existsDS("pie_to_eat", ".GlobalEnv", "numeric"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv","numeric"),TRUE)
  expect_equal(existsDS("pie_to_eat", ".GlobalEnv", "numeric"),FALSE)

  #integer
  assign("pie_to_eat",as.integer(3),envir=globalenv())
  expect_equal(existsDS("pie_to_eat", ".GlobalEnv", "integer"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = ".GlobalEnv","integer"),TRUE)
  expect_equal(existsDS("pie_to_eat", ".GlobalEnv", "integer"),FALSE)

  #character
  assign("name","coronavirus",envir=globalenv())
  expect_equal(existsDS("name", ".GlobalEnv", "character"),TRUE)
  expect_equal(removeDS("name",environment.name = ".GlobalEnv","character"),TRUE)
  expect_equal(existsDS("name", ".GlobalEnv", "character"),FALSE)

  #factor
  gender_vector <- c("Male", "Female", "Female", "Male", "Male","InterSex","Eunuch","Female")
  assign("factor",factor(gender_vector),envir=globalenv())
  expect_equal(existsDS("factor", ".GlobalEnv", "factor"),TRUE)
  expect_equal(removeDS("factor",environment.name = ".GlobalEnv","factor"),TRUE)
  expect_equal(existsDS("factor", ".GlobalEnv", "factor"),FALSE)

  age_vector <- c(4, 5, 34, 6, 7,0,9,4)
  assign("factor",factor(age_vector),envir=globalenv())
  expect_equal(existsDS("factor", ".GlobalEnv", "factor"),TRUE)
  expect_equal(removeDS("factor",environment.name = ".GlobalEnv","factor"),TRUE)
  expect_equal(existsDS("factor", ".GlobalEnv", "factor"),FALSE)

  #list
  assign("list_gender_age",list(gender = gender_vector, age = age_vector),envir=globalenv())
  expect_equal(existsDS("list_gender_age", ".GlobalEnv", "list"),TRUE)
  expect_equal(removeDS("list_gender_age",environment.name = ".GlobalEnv","list"),TRUE)
  expect_equal(existsDS("list_gender_age", ".GlobalEnv", "list"),FALSE)

  #matrix
  age_vector <- c(4, 5, 34, 6, 7,0,9,4)
  assign("matrix_age",matrix(age_vector, nrow=2, ncol=4), envir=globalenv())
  expect_equal(existsDS("matrix_age", ".GlobalEnv", "matrix"),TRUE)
  expect_equal(removeDS("matrix_age",environment.name = ".GlobalEnv","matrix"),TRUE)
  expect_equal(existsDS("matrix_age", ".GlobalEnv", "matrix"),FALSE)


  #environment
  assign("test_env",new.env(globalenv()), envir=globalenv())
  expect_equal(existsDS("test_env", ".GlobalEnv", "environment"),TRUE)
  expect_equal(removeDS("test_env",environment.name = ".GlobalEnv","environment"),TRUE)
  expect_equal(existsDS("test_env", ".GlobalEnv", "environment"),FALSE)

})

context("removeDS::expt::package:base")
test_that("failure",
{

  #null
  assign("null_value",NULL,envir=baseenv())
  expect_equal(existsDS("null_value", "package:base", "NULL"),TRUE)
  expect_equal(removeDS("null_value",environment.name = "package:base","NULL"),FALSE)
  expect_equal(existsDS("null_value", "package:base", "NULL"),TRUE)


  #logical
  assign("sunny_day",TRUE,envir=baseenv())
  expect_equal(existsDS("sunny_day", "package:base", "logical"),TRUE)
  expect_equal(removeDS("sunny_day",environment.name = "package:base","logical"),FALSE)
  expect_equal(existsDS("sunny_day", "package:base", "logical"),TRUE)

  #numeric
  assign("pie_to_eat",as.numeric(3.14),envir=baseenv())
  expect_equal(existsDS("pie_to_eat", "package:base", "numeric"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = "package:base","numeric"),FALSE)
  expect_equal(existsDS("pie_to_eat", "package:base", "numeric"),TRUE)

  #integer
  assign("pie_to_eat",as.integer(3),envir=baseenv())
  expect_equal(existsDS("pie_to_eat", "package:base", "integer"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = "package:base","integer"),FALSE)
  expect_equal(existsDS("pie_to_eat", "package:base", "integer"),TRUE)

  #character
  assign("name","coronavirus",envir=baseenv())
  expect_equal(existsDS("name", "package:base", "character"),TRUE)
  expect_equal(removeDS("name",environment.name = "package:base","character"),FALSE)
  expect_equal(existsDS("name", "package:base", "character"),TRUE)







  #matrix
  age_vector <- c(4, 5, 34, 6, 7,0,9,4)
  assign("matrix_age",matrix(age_vector, nrow=2, ncol=4), envir=baseenv())
  expect_equal(existsDS("matrix_age", "package:base", "matrix"),TRUE)
  expect_equal(removeDS("matrix_age",environment.name = "package:base","matrix"),FALSE)
  expect_equal(existsDS("matrix_age", "package:base", "matrix"),TRUE)


  #environment
  assign("test_env",new.env(baseenv()), envir=baseenv())
  expect_equal(existsDS("test_env", "package:base", "environment"),TRUE)
  expect_equal(removeDS("test_env",environment.name = "package:base","environment"),FALSE)
  expect_equal(existsDS("test_env", "package:base", "environment"),TRUE)

})

if(FALSE)
{
context("removeDS::expt::ds.test_env")
test_that("success",
{

  #null
  expect_equal(existsDS("null_value", "package:base", "NULL"),TRUE)
  expect_equal(removeDS("null_value",environment.name = "package:ds.server.parameter","NULL"),TRUE)
  expect_equal(existsDS("null_value", "package:ds.server.parameter", "NULL"),FALSE)


  #logical
  assign("sunny_day",TRUE,envir=globalenv())
  expect_equal(existsDS("sunny_day", "package:ds.server.parameter", "logical"),TRUE)
  expect_equal(removeDS("sunny_day",environment.name = "package:ds.server.parameter","logical"),TRUE)
  expect_equal(existsDS("sunny_day", "package:ds.server.parameter", "logical"),FALSE)

  #numeric
  assign("pie_to_eat",as.numeric(3.14),envir=globalenv())
  expect_equal(existsDS("pie_to_eat", "package:ds.server.parameter", "numeric"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = "package:ds.server.parameter","numeric"),TRUE)
  expect_equal(existsDS("pie_to_eat", "package:ds.server.parameter", "numeric"),FALSE)

  #integer
  assign("pie_to_eat",as.integer(3),envir=globalenv())
  expect_equal(existsDS("pie_to_eat", "package:ds.server.parameter", "integer"),TRUE)
  expect_equal(removeDS("pie_to_eat",environment.name = "package:ds.server.parameter","integer"),TRUE)
  expect_equal(existsDS("pie_to_eat", "package:ds.server.parameter", "integer"),FALSE)

  #character
  assign("name","coronavirus",envir=globalenv())
  expect_equal(existsDS("name", "package:ds.server.parameter", "character"),TRUE)
  expect_equal(removeDS("name",environment.name = "package:ds.server.parameter","character"),TRUE)
  expect_equal(existsDS("name", "package:ds.server.parameter", "character"),FALSE)

  #factor
  gender_vector <- c("Male", "Female", "Female", "Male", "Male","InterSex","Eunuch","Female")
  assign("factor",factor(gender_vector),envir=globalenv())
  expect_equal(existsDS("factor", "package:ds.server.parameter", "factor"),TRUE)
  expect_equal(removeDS("factor",environment.name = "package:ds.server.parameter","factor"),TRUE)
  expect_equal(existsDS("factor", "package:ds.server.parameter", "factor"),FALSE)

  age_vector <- c(4, 5, 34, 6, 7,0,9,4)
  assign("factor",factor(age_vector),envir=globalenv())
  expect_equal(existsDS("factor", "package:ds.server.parameter", "factor"),TRUE)
  expect_equal(removeDS("factor",environment.name = "package:ds.server.parameter","factor"),TRUE)
  expect_equal(existsDS("factor", "package:ds.server.parameter", "factor"),FALSE)

  #list
  assign("list_gender_age",list(gender = gender_vector, age = age_vector),envir=globalenv())
  expect_equal(existsDS("list_gender_age", "package:ds.server.parameter", "list"),TRUE)
  expect_equal(removeDS("list_gender_age",environment.name = "package:ds.server.parameter","list"),TRUE)
  expect_equal(existsDS("list_gender_age", "package:ds.server.parameter", "list"),FALSE)

  #matrix
  age_vector <- c(4, 5, 34, 6, 7,0,9,4)
  assign("matrix_age",matrix(age_vector, nrow=2, ncol=4), envir=globalenv())
  expect_equal(existsDS("matrix_age", "package:ds.server.parameter", "matrix"),TRUE)
  expect_equal(removeDS("matrix_age",environment.name = "package:ds.server.parameter","matrix"),TRUE)
  expect_equal(existsDS("matrix_age", "package:ds.server.parameter", "matrix"),FALSE)


  #environment
  assign("test_env",new.env(globalenv()), envir=globalenv())
  expect_equal(existsDS("test_env", "package:ds.server.parameter", "environment"),TRUE)
  expect_equal(removeDS("test_env",environment.name = "package:ds.server.parameter","environment"),TRUE)
  expect_equal(existsDS("test_env", "package:ds.server.parameter", "environment"),FALSE)

})

}
