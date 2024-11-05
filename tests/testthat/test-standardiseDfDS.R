library(DSLite)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)
library(purrr)
library(dplyr)

df <- create_mixed_dataframe()
df_list <- create_additional_dataframes(df)

df_1 <- df
df_2 <- df_list[[1]]
df_3 <- df_list[[2]]
df_4 <- df_list[[3]]

test_that("getClassAllColsDS returns correct classes", {
  expect_equal(
    getClassAllColsDS("df_1"),
    tibble(
      fac_col1 = "factor", fac_col2 = "factor", fac_col3 = "factor", fac_col4 = "factor", fac_col5 = "factor",
      fac_col6 = "factor", fac_col7 = "factor", fac_col8 = "factor", fac_col9 = "factor", fac_col10 = "factor",
      fac_col11 = "factor", fac_col12 = "factor", fac_col13 = "factor", fac_col14 = "factor", fac_col15 = "factor",
      col16 = "integer", col17 = "integer", col18 = "numeric", col19 = "numeric", col20 = "character",
      col21 = "character", col22 = "integer", col23 = "numeric", col24 = "character", col25 = "integer",
      col26 = "numeric", col27 = "character", col28 = "integer", col29 = "numeric", col30 = "character"
    )
  )
})

test_that("fixClassDS sets classes correctly", {

  cols_to_set <- c("fac_col13", "fac_col5", "col22", "col19", "col25", "col20", "col28",
                   "fac_col14", "fac_col3", "fac_col8")

  classes_to_set <- c("4", "1", "3", "5", "3", "2", "5", "5", "3", "2")

  expect_warning(
    classes_changed_df <- fixClassDS("df_1", cols_to_set, classes_to_set)
  )

  expect_equal(
    classes_changed_df %>% map_chr(class) %>% unname(),
    c("factor", "factor", "numeric", "factor", "factor", "factor", "factor", "integer", "factor",
      "factor", "factor", "factor", "character", "logical", "factor", "integer", "integer",
      "numeric", "logical", "integer", "character", "numeric", "numeric", "character", "numeric",
      "numeric", "character", "logical", "numeric", "character")
  )
})

test_that("convert_class calls the correct function", {

result <- .convertClass(c(1, 2, 3), "1")
expect_true(is.factor(result))

result <- .convertClass(c(1.5, 2.5, 3.7), "2")
expect_true(is.integer(result))

result <- .convertClass(c("1", "2", "3"), "3")
expect_true(is.numeric(result))

result <- .convertClass(c(1, 2, 3), "4")
expect_true(is.character(result))

result <- .convertClass(c(0, 1, 0), "5")
expect_true(is.logical(result))

})

test_that("fixColsDS correctly adds missing columns", {

  all_cols <- unique(c(colnames(df_1), colnames(df_2), colnames(df_3), colnames(df_4)))
  out <- fixColsDS("df_3", all_cols)

  expect_equal(
    colnames(out),
    sort(all_cols))

})

test_that("getAllLevelsDS correctly retrieves the levels of specified factor columns", {

  factor_vars <- c("fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col7",
                  "fac_col8", "fac_col9", "fac_col10", "fac_col11", "fac_col12", "fac_col14",
                  "fac_col15", "col27")

  observed <- getAllLevelsDS("df_3", factor_vars)

    expected <- list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Blue", "Green", "Red"),
      fac_col3 = c("No", "Yes"),
      fac_col4 = c("A", "B", "C"),
      fac_col5 = c("One", "Three", "Two"),
      fac_col6 = c("Bird", "Cat", "Dog"),
      fac_col7 = c("Large", "Medium", "Small"),
      fac_col8 = c("Alpha", "Beta", "Gamma"),
      fac_col9 = c("False", "True"),
      fac_col10 = c("Left", "Right"),
      fac_col11 = c("East", "North", "South", "West"),
      fac_col12 = c("Day", "Night"),
      fac_col14 = c("Female", "Male"),
      fac_col15 = c("Fall", "Spring", "Summer", "Winter"),
      col27 = letters
    )

  expect_equal(expected, observed)

})

example_df <- data.frame(
  col1 = c("A", "B", "A", "C"),
  col2 = c("X", "Y", "X", "Z"),
  col3 = c("Yes", "No", "Yes", "No"),
  stringsAsFactors = FALSE
)

test_that("fixLevelsDS sets factor levels correctly", {

  levels <- list(
    col1 = c("A", "B", "C"),
    col2 = c("X", "Y", "Z"),
    col3 = c("Yes", "No")
  )

  modified_df <- fixLevelsDS("example_df", c("col1", "col2", "col3"), levels)

  expect_s3_class(modified_df$col1, "factor")
  expect_s3_class(modified_df$col2, "factor")
  expect_s3_class(modified_df$col3, "factor")

  expect_equal(levels(modified_df$col1), levels$col1)
  expect_equal(levels(modified_df$col2), levels$col2)
  expect_equal(levels(modified_df$col3), levels$col3)

})

test_that("fixLevelsDS throws an error for invalid input", {

  levels <- list(
    col1 = c("A", "B", "C"),
    col2 = c("X", "Y", "Z")
  )

  expect_error(fixLevelsDS("example_df", c("col1", "non_existent_col"), levels))
})
