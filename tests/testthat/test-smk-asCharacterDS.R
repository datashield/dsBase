test_that("asCharacterDS coerces numeric to character", {
  input <- c(1.0, 2.5, 3.0)

  res <- asCharacterDS("input")

  expect_equal(class(res), "character")
  expect_equal(res, as.character(input))
})

test_that("asCharacterDS coerces integer to character", {
  input <- as.integer(c(1, 2, 3))

  res <- asCharacterDS("input")

  expect_equal(class(res), "character")
})

test_that("asCharacterDS throws error when object does not exist", {
  expect_error(
    asCharacterDS("nonexistent_object"),
    regexp = "does not exist"
  )
})
