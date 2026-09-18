
#
# Set up
#

# context("dmtC2SDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple dmtC2SDS returns matrix", {
    res <- dmtC2SDS(
        dfdata.mat.transmit   = "1,2,3,4",
        inout.object.transmit = "MAT",
        from                  = "clientside.matdftbl",
        nrows.transmit        = "2",
        ncols.transmit        = "2",
        colnames.transmit     = "a,b",
        colclass.transmit     = "numeric,numeric",
        byrow                 = FALSE
    )

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 2)
    expect_equal(ncol(res), 2)
    expect_equal(colnames(res), c("a", "b"))
})
