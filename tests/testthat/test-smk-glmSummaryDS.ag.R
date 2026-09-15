#
# Set up
#

# context("glmSummaryDS.ag::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmSummaryDS.ag::smk::gaussian")
test_that("simple glmSummaryDS.ag, gaussian", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )
    my.glm.obj <- glm(y ~ x, family = gaussian(), data = D)

    res <- glmSummaryDS.ag(x.transmit = "my.glm.obj")

    expect_equal(class(res), "list")
    expect_length(res, 2)

    expect_equal(class(res$glm.obj), c("glm", "lm"))
    expect_equal(as.numeric(res$glm.obj$coefficients), c(-5.7261526, 0.2317703), tolerance = 1e-6)
    expect_true(is.na(res$glm.obj$residuals))
    expect_true(is.na(res$glm.obj$fitted.values))
    expect_true(is.na(res$glm.obj$y))
    expect_true(is.na(res$glm.obj$x))
    expect_equal(res$glm.obj$data, c("y", "x"))

    expect_equal(class(res$glm.summary.obj), "summary.glm")
    expect_length(res$glm.summary.obj, 18)
    expect_true(is.na(res$glm.summary.obj$na.action))
    expect_true(is.na(res$glm.summary.obj$deviance.resid))
    expect_equal(dim(res$glm.summary.obj$coefficients), c(2, 4))
    expect_equal(res$glm.summary.obj$coefficients[, "Estimate"],
                 c(`(Intercept)` = -5.7261526, x = 0.2317703), tolerance = 1e-6)
})

test_that("glmSummaryDS.ag throws error when x does not exist", {
    expect_error(
        glmSummaryDS.ag(x.transmit = "nonexistent_object"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("glmSummaryDS.ag::smk::shutdown")

# context("glmSummaryDS.ag::smk::done")
