test_that("asListDS coerces data.frame to list", {
  input <- data.frame(v1 = c(1.0, 2.0), v2 = c(3.0, 4.0))

  res <- asListDS("input", "test_output")

  expect_true(is.list(res))
  expect_true(grepl("New object <test_output> created", res$return.message))
  expect_true(grepl("list", res$class.of.newobj))
})

test_that("asListDS coerces vector to list", {
  input <- c(1, 2, 3)

  res <- asListDS("input", "test_output2")

  expect_true(is.list(res))
  expect_true(grepl("New object <test_output2> created", res$return.message))
})

test_that("asListDS throws error when object does not exist", {
  expect_error(
    asListDS("nonexistent_object", "test_output"),
    regexp = "does not exist"
  )
})
