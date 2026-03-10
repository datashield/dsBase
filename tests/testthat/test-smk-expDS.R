test_that("expDS computes exponential for numeric vector", {
  input <- c(0.0, 1.0, 2.0, -1.0)

  res <- expDS("input")

  expect_equal(res, exp(input))
  expect_true(is.numeric(res))
})

test_that("expDS computes exponential for integer vector", {
  input <- as.integer(c(0, 1, 2, 3))

  res <- expDS("input")

  expect_equal(res, exp(input))
})

test_that("expDS throws error when object does not exist", {
  expect_error(
    expDS("nonexistent_object"),
    regexp = "does not exist"
  )
})

test_that("expDS throws error when object is not numeric or integer", {
  bad_input <- c("a", "b", "c")
  expect_error(
    expDS("bad_input"),
    regexp = "must be of type"
  )
})
