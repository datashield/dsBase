
#
# Set up
#

# context("reShapeDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple reShapeDS wide to long", {
    wide_df <- data.frame(
        id    = 1:3,
        sbp.1 = c(120, 130, 125),
        sbp.2 = c(122, 135, 128)
    )

    res <- reShapeDS(
        data.name         = "wide_df",
        varying.transmit  = "sbp.1,sbp.2",
        v.names.transmit  = "sbp",
        timevar.name      = "time",
        idvar.name        = "id",
        drop.transmit     = NULL,
        direction         = "long",
        sep               = "."
    )

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 6)
    expect_true("sbp" %in% colnames(res))
    expect_true("time" %in% colnames(res))
})

test_that("reShapeDS errors when serverside object does not exist", {
    expect_error(reShapeDS(
        data.name         = "nonexistent_object",
        varying.transmit  = "sbp.1,sbp.2",
        v.names.transmit  = "sbp",
        timevar.name      = "time",
        idvar.name        = "id",
        drop.transmit     = NULL,
        direction         = "long",
        sep               = "."
    ), regexp = "does not exist")
})
