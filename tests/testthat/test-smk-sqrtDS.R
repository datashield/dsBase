test_that("sqrtDS computes square root for numeric vector", {
  input <- c(4.0, 9.0, 16.0, 25.0)

  res <- sqrtDS("input")

  expect_equal(res, sqrt(input))
  expect_true(is.numeric(res))
})

test_that("sqrtDS computes square root for integer vector", {
  input <- as.integer(c(1, 4, 9, 16))

  res <- sqrtDS("input")

  expect_equal(res, sqrt(input))
})

test_that("sqrtDS throws error when object does not exist", {
  expect_error(
    sqrtDS("nonexistent_object"),
    regexp = "does not exist"
  )
})

test_that("sqrtDS throws error when object is not numeric or integer", {
  bad_input <- c("a", "b", "c")
  expect_error(
    sqrtDS("bad_input"),
    regexp = "must be of type"
  )
})
