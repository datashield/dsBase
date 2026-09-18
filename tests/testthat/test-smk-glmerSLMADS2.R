#
# Set up
#

# context("glmerSLMADS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmerSLMADS2::smk::binomial random intercept")
test_that("simple glmerSLMADS2, binomial random intercept", {
    set.seed(7)
    D <- data.frame(
        group = factor(rep(1:5, each = 10)),
        x = rep(1:10, times = 5)
    )
    lp <- -1.5 + 0.35 * D$x + rep(rnorm(5, sd = 1.5), each = 10)
    D$y <- rbinom(50, 1, plogis(lp))

    res <- suppressWarnings(
        glmerSLMADS2(formula = stats::as.formula("y ~ x + (1|group)"),
                    offset = NULL, weights = NULL, dataName = "D", family = "binomial",
                    control_type = NULL, control_value.transmit = NULL,
                    nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL)
    )

    expect_equal(class(res), "summary.merMod")
    expect_equal(res$errorMessage, "No errors")
    expect_equal(res$disclosure.risk, 0)
    expect_equal(res$Ntotal, 50)
    expect_equal(res$Nvalid, 50)
    expect_equal(res$Nmissing, 0)
    expect_equal(dim(res$coefficients), c(2, 4))
    expect_equal(res$coefficients[, "Estimate"],
                 c(`(Intercept)` = -1.4299523, x = 0.2248647), tolerance = 1e-5)
    expect_equal(res$family, "binomial")
})

test_that("glmerSLMADS2 throws error when dataName does not exist", {
    expect_error(
        glmerSLMADS2(formula = stats::as.formula("y ~ x + (1|group)"),
                    offset = NULL, weights = NULL, dataName = "nonexistent_object", family = "binomial",
                    control_type = NULL, control_value.transmit = NULL,
                    nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL),
        regexp = "does not exist"
    )
})

test_that("glmerSLMADS2 throws error when dataName is not a data.frame or matrix", {
    bad_input <- c(1, 2, 3)
    expect_error(
        glmerSLMADS2(formula = stats::as.formula("y ~ x + (1|group)"),
                    offset = NULL, weights = NULL, dataName = "bad_input", family = "binomial",
                    control_type = NULL, control_value.transmit = NULL,
                    nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("glmerSLMADS2::smk::shutdown")

# context("glmerSLMADS2::smk::done")
