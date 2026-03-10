test_that("logDS computes natural log for numeric vector", {
  input <- c(1.0, exp(1), exp(2))

  res <- logDS("input")

  expect_equal(res, log(input))
  expect_true(is.numeric(res))
})

test_that("logDS computes log with custom base", {
  input <- c(1.0, 10.0, 100.0)

  res <- logDS("input", base = 10)

  expect_equal(res, log(input, base = 10))
})

test_that("logDS computes log for integer vector", {
  input <- as.integer(c(1, 2, 3, 4))

  res <- logDS("input")

  expect_equal(res, log(input))
})

test_that("logDS throws error when object does not exist", {
  expect_error(
    logDS("nonexistent_object"),
    regexp = "does not exist"
  )
})

test_that("logDS throws error when object is not numeric or integer", {
  bad_input <- c("a", "b", "c")
  expect_error(
    logDS("bad_input"),
    regexp = "must be of type"
  )
})
