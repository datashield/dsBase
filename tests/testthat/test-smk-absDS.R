test_that("absDS computes absolute values for numeric vector", {
  input <- c(-3.5, -1.0, 0.0, 2.5, 4.0)

  res <- absDS("input")

  expect_equal(res, abs(input))
  expect_true(is.numeric(res))
})

test_that("absDS computes absolute values for integer vector", {
  input <- as.integer(c(-5, -3, 0, 2, 7))

  res <- absDS("input")

  expect_equal(res, abs(input))
  expect_true(is.integer(res))
})

test_that("absDS throws error when object does not exist", {
  expect_error(
    absDS("nonexistent_object"),
    regexp = "does not exist"
  )
})

test_that("absDS throws error when object is not numeric or integer", {
  bad_input <- c("a", "b", "c")
  expect_error(
    absDS("bad_input"),
    regexp = "must be of type"
  )
})
