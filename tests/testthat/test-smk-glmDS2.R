#
# Set up
#

# context("glmDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmDS2::smk::gaussian")
test_that("simple glmDS2, gaussian", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )

    expect_warning(
        res <- glmDS2(formula = stats::as.formula("y ~ x"), family = "gaussian",
                      beta.vect = "0,0", offset = NULL, weights = NULL, dataName = "D"),
        regexp = "did not converge"
    )

    expect_equal(class(res), "list")
    expect_length(res, 10)
    expect_equal(res$family$family, "gaussian")
    expect_equal(res$family$link, "identity")
    expect_equal(dim(res$info.matrix), c(2, 2))
    expect_equal(as.numeric(res$info.matrix), c(20, 791.4, 791.4, 31369.1), tolerance = 1e-6)
    expect_equal(dim(res$score.vect), c(2, 1))
    expect_equal(as.numeric(res$score.vect), c(68.9, 2738.75), tolerance = 1e-6)
    expect_equal(res$numsubs, 20)
    expect_equal(res$dev, 243, tolerance = 1)
    expect_equal(res$Nvalid, 20)
    expect_equal(res$Nmissing, 0)
    expect_equal(res$Ntotal, 20)
    expect_equal(res$disclosure.risk, 0)
    expect_equal(res$errorMessage2, "No errors")
})

test_that("glmDS2 throws error when dataName does not exist", {
    expect_error(
        glmDS2(formula = stats::as.formula("y ~ x"), family = "gaussian",
              beta.vect = "0,0", offset = NULL, weights = NULL, dataName = "nonexistent_object"),
        regexp = "does not exist"
    )
})

test_that("glmDS2 throws error when dataName is not a data.frame or matrix", {
    bad_input <- c(1, 2, 3)
    expect_error(
        glmDS2(formula = stats::as.formula("y ~ x"), family = "gaussian",
              beta.vect = "0,0", offset = NULL, weights = NULL, dataName = "bad_input"),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("glmDS2::smk::shutdown")

# context("glmDS2::smk::done")
