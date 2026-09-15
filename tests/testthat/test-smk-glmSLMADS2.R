#
# Set up
#

# context("glmSLMADS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmSLMADS2::smk::gaussian")
test_that("simple glmSLMADS2, gaussian", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )
    my.glm.obj <- glm(y ~ x, family = gaussian(), data = D, x = TRUE)

    res <- glmSLMADS2(formula = stats::as.formula("y ~ x"), family = "gaussian",
                      offset = NULL, weights = NULL, newobj = "my.glm.obj", dataName = "D")

    expect_equal(class(res), "list")
    expect_length(res, 29)
    expect_equal(res$rank, 2)
    expect_equal(res$aic, 23.15387, tolerance = 1e-5)
    expect_equal(res$iter, 2)
    expect_true(res$converged)
    expect_false(res$boundary)
    expect_equal(res$data, "D")
    expect_equal(res$Ntotal, 20)
    expect_equal(res$Nvalid, 20)
    expect_equal(res$Nmissing, 0)
    expect_equal(dim(res$coefficients), c(2, 4))
    expect_equal(rownames(res$coefficients), c("(Intercept)", "x"))
    expect_equal(res$coefficients[, "Estimate"], c(`(Intercept)` = -5.7261526, x = 0.2317703), tolerance = 1e-6)
    expect_equal(res$family$family, "gaussian")
})

test_that("glmSLMADS2 throws error when dataName does not exist", {
    # dataName is validated before newobj is loaded, so no glm object needs to exist here.
    expect_error(
        glmSLMADS2(formula = stats::as.formula("y ~ x"), family = "gaussian",
                  offset = NULL, weights = NULL, newobj = "my.glm.obj", dataName = "nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("glmSLMADS2 throws error when dataName is not a data.frame or matrix", {
    bad_input <- c(1, 2, 3)

    expect_error(
        glmSLMADS2(formula = stats::as.formula("y ~ x"), family = "gaussian",
                  offset = NULL, weights = NULL, newobj = "my.glm.obj", dataName = "bad_input"),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("glmSLMADS2::smk::shutdown")

# context("glmSLMADS2::smk::done")
