test_that("asIntegerDS coerces numeric to integer", {
  input <- c(1.0, 2.0, 3.0)

  res <- asIntegerDS("input")

  expect_equal(class(res), "integer")
  expect_equal(res, as.integer(input))
})

test_that("asIntegerDS coerces factor with numeric levels correctly", {
  input <- factor(c(0, 1, 1, 2))

  res <- asIntegerDS("input")

  expect_equal(class(res), "integer")
  expect_equal(res, c(0L, 1L, 1L, 2L))
})

test_that("asIntegerDS throws error when object does not exist", {
  expect_error(
    asIntegerDS("nonexistent_object"),
    regexp = "does not exist"
  )
})
