test_that("asNumericDS coerces integer to numeric", {
  input <- as.integer(c(1, 2, 3))

  res <- asNumericDS("input")

  expect_equal(class(res), "numeric")
  expect_equal(res, c(1, 2, 3))
})

test_that("asNumericDS coerces factor with numeric levels correctly", {
  input <- factor(c(0, 1, 1, 2))

  res <- asNumericDS("input")

  expect_equal(class(res), "numeric")
  expect_equal(res, c(0, 1, 1, 2))
})

test_that("asNumericDS coerces character with numeric strings correctly", {
  input <- c("1", "2", "3")

  res <- asNumericDS("input")

  expect_equal(class(res), "numeric")
  expect_equal(res, c(1, 2, 3))
})

test_that("asNumericDS throws error when object does not exist", {
  expect_error(
    asNumericDS("nonexistent_object"),
    regexp = "does not exist"
  )
})
