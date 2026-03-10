test_that("asLogicalDS coerces numeric to logical", {
  input <- c(0, 1, 0, 1, 1)

  res <- asLogicalDS("input")

  expect_equal(class(res), "logical")
  expect_equal(res, as.logical(input))
})

test_that("asLogicalDS coerces integer to logical", {
  input <- as.integer(c(0, 1, 0))

  res <- asLogicalDS("input")

  expect_equal(class(res), "logical")
})

test_that("asLogicalDS coerces character to logical", {
  input <- c("TRUE", "FALSE", "TRUE")

  res <- asLogicalDS("input")

  expect_equal(class(res), "logical")
  expect_equal(res, c(TRUE, FALSE, TRUE))
})

test_that("asLogicalDS throws error when object does not exist", {
  expect_error(
    asLogicalDS("nonexistent_object"),
    regexp = "does not exist"
  )
})

test_that("asLogicalDS throws error when object is not permitted type", {
  bad_input <- data.frame(a = 1:3)
  expect_error(
    asLogicalDS("bad_input"),
    regexp = "must be of type"
  )
})
