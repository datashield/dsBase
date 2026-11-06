#
# Set up
#

# context("histogram1DS::smk::setup")

set.standard.disclosure.settings()
set.random.seed.setting(1234)

#
# Tests
#

# context("histogram1DS::smk")
test_that("histogram1DS, smallCellsRule method", {
    xvect <- c(1:100)

    res <- histogram1DS("xvect", method.indicator=1, k=3, noise=0.25)

    expect_length(res, 2)
    expect_equal(res$class, "integer")
    expect_length(res$range, 2)
})

test_that("histogram1DS fails when x references nonexistent object", {
    expect_error(histogram1DS("nonexistent_obj", method.indicator=1, k=3, noise=0.25), "does not exist")
})

test_that("histogram1DS fails when x is not numeric or integer", {
    xvect <- c("a", "b", "c")
    expect_error(histogram1DS("xvect", method.indicator=1, k=3, noise=0.25), "must be of type numeric or integer")
})

test_that("histogram1DS gives the same result for numeric input as for integer", {
    x.integer <- c(1:100)
    x.numeric <- as.numeric(x.integer)

    res.integer <- histogram1DS("x.integer", method.indicator=1, k=3, noise=0.25)
    res.numeric <- histogram1DS("x.numeric", method.indicator=1, k=3, noise=0.25)

    expect_equal(res.numeric$class, "numeric")
    expect_equal(res.numeric[names(res.numeric) != "class"], res.integer[names(res.integer) != "class"])
})

#
# Done
#

# context("histogram1DS::smk::shutdown")

# context("histogram1DS::smk::done")
