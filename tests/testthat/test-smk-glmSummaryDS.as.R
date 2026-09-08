#
# Set up
#

# context("glmSummaryDS.as::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmSummaryDS.as::smk::gaussian")
test_that("simple glmSummaryDS.as, gaussian", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )
    my.glm.obj <- glm(y ~ x, family = gaussian(), data = D)

    res <- glmSummaryDS.as(x.transmit = "my.glm.obj")

    expect_equal(class(res), "summary.glm")
    expect_length(res, 17)
    expect_equal(res$aic, 23.15387, tolerance = 1e-5)
    expect_equal(res$df.residual, 18)
    expect_equal(res$null.deviance, 5.6295, tolerance = 1e-5)

    expect_true(is.na(res$deviance.resid))
    expect_null(res$na.action)
    expect_equal(dim(res$coefficients), c(2, 4))
    expect_equal(rownames(res$coefficients), c("(Intercept)", "x"))
    expect_equal(res$coefficients[, "Estimate"], c(`(Intercept)` = -5.7261526, x = 0.2317703), tolerance = 1e-6)
})

test_that("glmSummaryDS.as throws error when x does not exist", {
    expect_error(
        glmSummaryDS.as(x.transmit = "nonexistent_object"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("glmSummaryDS.as::smk::shutdown")

# context("glmSummaryDS.as::smk::done")
