#
# Set up
#

# context("tableDS.assign::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("tableDS.assign::smk")
test_that("simple tableDS.assign", {
    rvar <- factor(rep(c("a", "b"), each = 10))
    cvar <- factor(rep(rep(c("x", "y"), each = 5), 2))

    res <- tableDS.assign("rvar", "cvar", NULL, "a,b", "x,y", NULL, NULL, "no")

    expect_equal(class(res), "list")
    expect_equal(names(res), c("table", "counts", "dim", "dimnames"))
    expect_equal(res$dim, c(2L, 2L))
    expect_equal(res$counts, c(5, 5, 5, 5))
})

test_that("tableDS.assign throws error when object does not exist", {
    expect_error(
        tableDS.assign("nonexistent_object", NULL, NULL, "a,b", NULL, NULL, NULL, "no"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("tableDS.assign::smk::shutdown")

# context("tableDS.assign::smk::done")
