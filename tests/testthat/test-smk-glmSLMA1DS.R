#
# Set up
#

# context("glmSLMA1DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmSLMA1DS::smk::gaussian")
test_that("simple glmSLMA1DS, gaussian", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )

    expect_warning(
        res <- glmSLMA1DS(formula = stats::as.formula("y ~ x"), family = "gaussian",
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

test_that("glmSLMA1DS throws error when data does not exist", {
    expect_error(
        glmSLMA1DS(formula = stats::as.formula("y ~ x"), family = "gaussian",
                  weights = NULL, offset = NULL, data = "nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("glmSLMA1DS throws error when data is not a data.frame or list", {
    bad_input <- c(1, 2, 3)
    expect_error(
        glmSLMA1DS(formula = stats::as.formula("y ~ x"), family = "gaussian",
                  weights = NULL, offset = NULL, data = "bad_input"),
        regexp = "must be of type"
    )
})

test_that("glmSLMA1DS gives the same result for data as a list as for a data.frame", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )
    D.list <- as.list(D)

    res.df <- suppressWarnings(glmSLMA1DS(formula = stats::as.formula("y ~ x"), family = "gaussian", weights = NULL, offset = NULL, data = "D"))
    res.list <- suppressWarnings(glmSLMA1DS(formula = stats::as.formula("y ~ x"), family = "gaussian", weights = NULL, offset = NULL, data = "D.list"))

    expect_equal(res.list, res.df)
})

#
# Done
#

# context("glmSLMA1DS::smk::shutdown")

# context("glmSLMA1DS::smk::done")
