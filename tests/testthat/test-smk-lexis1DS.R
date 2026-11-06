#
# Set up
#

# context("lexis1DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lexis1DS::smk")
test_that("simple lexis1DS", {
    input <- c(1, 5, 3, 9, 2)

    res <- lexis1DS("input")

    expect_equal(class(res), "list")
    expect_equal(names(res), "max.time")
    # max.time is the max exposure time masked by a random +1%-5% increment
    expect_gte(res$max.time, 9 * 1.01)
    expect_lte(res$max.time, 9 * 1.05)
})

test_that("lexis1DS throws error when object does not exist", {
    expect_error(
        lexis1DS("nonexistent_object"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("lexis1DS::smk::shutdown")

# context("lexis1DS::smk::done")
