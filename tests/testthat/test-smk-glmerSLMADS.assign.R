#
# Set up
#

# context("glmerSLMADS.assign::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmerSLMADS.assign::smk::binomial random intercept")
test_that("simple glmerSLMADS.assign, binomial random intercept", {
    set.seed(7)
    D <- data.frame(
        group = factor(rep(1:5, each = 10)),
        x = rep(1:10, times = 5)
    )
    lp <- -1.5 + 0.35 * D$x + rep(rnorm(5, sd = 1.5), each = 10)
    D$y <- rbinom(50, 1, plogis(lp))

    res <- suppressWarnings(
        glmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                           offset = NULL, weights = NULL, dataName = "D", family = "binomial",
                           control_type = NULL, control_value.transmit = NULL,
                           nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL)
    )

    expect_true(methods::is(res, "glmerMod"))
    expect_equal(nobs(res), 50)
    expect_equal(as.numeric(lme4::fixef(res)), c(-1.4340652, 0.2255225), tolerance = 1e-5)

    vc <- as.data.frame(lme4::VarCorr(res))
    expect_equal(vc$vcov, 2.7493590, tolerance = 1e-5)
})

test_that("glmerSLMADS.assign throws error when dataName does not exist", {
    expect_error(
        glmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                           offset = NULL, weights = NULL, dataName = "nonexistent_object", family = "binomial",
                           control_type = NULL, control_value.transmit = NULL,
                           nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL),
        regexp = "does not exist"
    )
})

test_that("glmerSLMADS.assign throws error when dataName is not a data.frame or list", {
    bad_input <- c(1, 2, 3)
    expect_error(
        glmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                           offset = NULL, weights = NULL, dataName = "bad_input", family = "binomial",
                           control_type = NULL, control_value.transmit = NULL,
                           nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL),
        regexp = "must be of type"
    )
})

test_that("glmerSLMADS.assign gives the same result for data as a list as for a data.frame", {
    set.seed(7)
    D <- data.frame(
        group = factor(rep(1:5, each = 10)),
        x = rep(1:10, times = 5)
    )
    lp <- -1.5 + 0.35 * D$x + rep(rnorm(5, sd = 1.5), each = 10)
    D$y <- rbinom(50, 1, plogis(lp))
    D.list <- as.list(D)

    res.df <- suppressWarnings(glmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
        offset = NULL, weights = NULL, dataName = "D", family = "binomial",
        control_type = NULL, control_value.transmit = NULL,
        nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL))
    res.list <- suppressWarnings(glmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
        offset = NULL, weights = NULL, dataName = "D.list", family = "binomial",
        control_type = NULL, control_value.transmit = NULL,
        nAGQ = 1L, verbose = 0, theta = NULL, fixef = NULL))

    expect_equal(lme4::fixef(res.list), lme4::fixef(res.df))
})

#
# Done
#

# context("glmerSLMADS.assign::smk::shutdown")

# context("glmerSLMADS.assign::smk::done")
