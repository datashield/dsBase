test_that("asDataMatrixDS coerces data.frame to matrix", {
  input <- data.frame(v1 = c(1.0, 2.0, 3.0), v2 = c(4.0, 5.0, 6.0))

  res <- asDataMatrixDS("input")

  expect_true(is.matrix(res))
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 2)
})

test_that("asDataMatrixDS throws error when object does not exist", {
  expect_error(
    asDataMatrixDS("nonexistent_object"),
    regexp = "does not exist"
  )
})
