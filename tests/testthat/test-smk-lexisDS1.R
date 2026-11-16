#
# Set up
#

# context("lexisDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lexisDS1::smk")
test_that("simple lexisDS1", {
    input <- c(1, 5, 3, 9, 2)

    res <- lexisDS1("input")

    expect_equal(class(res), "list")
    expect_equal(names(res), "max.time")
    # max.time is the max exposure time masked by a random +1%-5% increment
    expect_gte(res$max.time, 9 * 1.01)
    expect_lte(res$max.time, 9 * 1.05)
})

test_that("lexisDS1 throws error when object does not exist", {
    expect_error(
        lexisDS1("nonexistent_object"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("lexisDS1::smk::shutdown")

# context("lexisDS1::smk::done")
