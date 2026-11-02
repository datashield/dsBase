#
# Set up
#

# context("table2DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("table2DS::smk")
test_that("simple table2DS", {
    rvar <- factor(rep(c("a", "b"), each = 10))
    cvar <- factor(rep(rep(c("x", "y"), each = 5), 2))
    res.assign <- tableDS.assign("rvar", "cvar", NULL, "a,b", "x,y", NULL, NULL, "no")

    res <- table2DS("res.assign", "rvar", "cvar", NULL)

    expect_equal(class(res), "list")
    expect_equal(
        names(res),
        c("table.cell.IDs", "table.dim", "table.dimnames", "table.structure_and_cell.order")
    )
    expect_equal(res$table.dim, c(2L, 2L))
    expect_equal(res$table.cell.IDs, 1:4)
})

test_that("table2DS throws error when object does not exist", {
    expect_error(
        table2DS("nonexistent_object", "rvar", "cvar", NULL),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("table2DS::smk::shutdown")

# context("table2DS::smk::done")
