#
# Set up
#

# context("glmSLMADS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmSLMADS1::smk::gaussian")
test_that("simple glmSLMADS1, gaussian", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )

    expect_warning(
        res <- glmSLMADS1(formula = stats::as.formula("y ~ x"), family = "gaussian",
                          weights = NULL, offset = NULL, data = "D"),
        regexp = "did not converge"
    )

    expect_equal(class(res), "list")
    expect_length(res, 8)
    expect_equal(res$dimX, c(20, 2))
    expect_equal(res$coef.names, c("(Intercept)", "x"))
    expect_equal(res$y.invalid, 0)
    expect_equal(res$Xpar.invalid, c(0, 0))
    expect_equal(res$w.invalid, 0)
    expect_equal(res$o.invalid, 0)
    expect_equal(res$glm.saturation.invalid, 0)
    expect_equal(res$errorMessage, "No errors")
})

test_that("glmSLMADS1 throws error when data does not exist", {
    expect_error(
        glmSLMADS1(formula = stats::as.formula("y ~ x"), family = "gaussian",
                  weights = NULL, offset = NULL, data = "nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("glmSLMADS1 throws error when data is not a data.frame or matrix", {
    bad_input <- c(1, 2, 3)
    expect_error(
        glmSLMADS1(formula = stats::as.formula("y ~ x"), family = "gaussian",
                  weights = NULL, offset = NULL, data = "bad_input"),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("glmSLMADS1::smk::shutdown")

# context("glmSLMADS1::smk::done")
