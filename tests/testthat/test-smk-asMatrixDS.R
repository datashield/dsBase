test_that("asMatrixDS coerces data.frame to matrix", {
  input <- data.frame(v1 = c(1.0, 2.0, 3.0), v2 = c(4.0, 5.0, 6.0))

  res <- asMatrixDS("input")

  expect_true(is.matrix(res))
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 2)
})

test_that("asMatrixDS coerces vector to matrix", {
  input <- c(1, 2, 3, 4)

  res <- asMatrixDS("input")

  expect_true(is.matrix(res))
})

test_that("asMatrixDS throws error when object does not exist", {
  expect_error(
    asMatrixDS("nonexistent_object"),
    regexp = "does not exist"
  )
})
