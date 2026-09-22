#
# Set up
#

# context("tableDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("tableDS::smk::1D")
test_that("simple tableDS, 1D", {
    input <- factor(rep(c("a", "b"), each = 5))

    res <- tableDS("input", NULL, NULL, "a,b", NULL, NULL, NULL, "no", NULL)

    expect_equal(class(res), "table")
    expect_equal(as.vector(res), c(5, 5))
    expect_equal(names(res), c("a", "b"))
})

# context("tableDS::smk::2D")
test_that("simple tableDS, 2D", {
    rvar <- factor(rep(c("a", "b"), each = 10))
    cvar <- factor(rep(rep(c("x", "y"), each = 5), 2))

    res <- tableDS("rvar", "cvar", NULL, "a,b", "x,y", NULL, NULL, "no", NULL)

    expect_equal(class(res), "table")
    expect_equal(dim(res), c(2L, 2L))
    expect_equal(as.vector(res), c(5, 5, 5, 5))
})

test_that("tableDS throws error when object does not exist", {
    expect_error(
        tableDS("nonexistent_object", NULL, NULL, "a,b", NULL, NULL, NULL, "no", NULL),
        regexp = "does not exist"
    )
})

test_that("tableDS accepts a forced nfilter given as a number", {
    input <- factor(rep(c("a", "b"), each = 5))

    res <- tableDS("input", NULL, NULL, "a,b", NULL, NULL, NULL, "no", "3")

    expect_equal(class(res), "table")
    expect_equal(as.vector(res), c(5, 5))
})

test_that("tableDS throws error when forced nfilter is below nfilter.tab", {
    input <- factor(rep(c("a", "b"), each = 5))

    expect_error(
        tableDS("input", NULL, NULL, "a,b", NULL, NULL, NULL, "no", "1"),
        regexp = "force.nfilter is non-null it must be >= to nfilter.tab"
    )
})

test_that("tableDS excludes levels given as literal values", {
    input <- factor(rep(c("a", "b", "c"), each = 5))

    res <- tableDS("input", NULL, NULL, "a,b,c", NULL, NULL, "a", "no", NULL)

    expect_equal(names(res), c("b", "c"))
    expect_equal(as.vector(res), c(5, 5))
})

test_that("tableDS treats NA in a list of excluded values as missing", {
    input <- factor(c(rep(c("a", "b"), each = 5), rep(NA, 5)))

    expect_warning(
        res <- tableDS("input", NULL, NULL, "a,b", NULL, NULL, "a,NA", "always", NULL),
        regexp = "'exclude' containing NA"
    )

    expect_equal(names(res), "b")
    expect_equal(as.vector(res), 5)
})

#
# Done
#

# context("tableDS::smk::shutdown")

# context("tableDS::smk::done")
