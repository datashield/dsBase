#
# Set up
#

# context("lmerSLMADS.assign::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lmerSLMADS.assign::smk::random intercept")
test_that("simple lmerSLMADS.assign, random intercept", {
    set.seed(42)
    D <- data.frame(
        group = factor(rep(1:4, each = 6)),
        x = rep(1:6, times = 4)
    )
    D$y <- 2 + 0.5 * D$x + rep(rnorm(4, sd = 1), each = 6) + rnorm(24, sd = 0.3)

    res <- suppressWarnings(
        lmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                          offset = NULL, weights = NULL, dataName = "D", REML = TRUE,
                          control_type = NULL, control_value.transmit = NULL,
                          optimizer = NULL, verbose = 0)
    )

    expect_true(methods::is(res, "lmerMod"))
    expect_equal(nobs(res), 24)
    expect_equal(as.numeric(lme4::fixef(res)), c(2.5942648, 0.4604968), tolerance = 1e-5)
    expect_equal(sigma(res), 0.3874094, tolerance = 1e-5)

    vc <- as.data.frame(lme4::VarCorr(res))
    expect_equal(vc$vcov, c(0.7137558, 0.1500860), tolerance = 1e-5)
})

# context("lmerSLMADS.assign::smk::weighted")
test_that("simple lmerSLMADS.assign, with weights", {
    set.seed(42)
    D <- data.frame(
        group = factor(rep(1:4, each = 6)),
        x = rep(1:6, times = 4)
    )
    D$y <- 2 + 0.5 * D$x + rep(rnorm(4, sd = 1), each = 6) + rnorm(24, sd = 0.3)
    D$w <- rep(c(1, 2), times = 12)

    res <- suppressWarnings(
        lmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                          offset = NULL, weights = "D$w", dataName = "D", REML = TRUE,
                          control_type = NULL, control_value.transmit = NULL,
                          optimizer = NULL, verbose = 0)
    )

    expect_true(methods::is(res, "lmerMod"))
    expect_equal(nobs(res), 24)
    expect_equal(as.numeric(lme4::fixef(res)), c(2.62267795897883, 0.4501480370479), tolerance = 1e-5)
    expect_equal(sigma(res), 0.4786046, tolerance = 1e-5)

    vc <- as.data.frame(lme4::VarCorr(res))
    expect_equal(vc$vcov, c(0.631006576026455, 0.22906240776478), tolerance = 1e-5)
})

test_that("lmerSLMADS.assign throws error when dataName does not exist", {
    expect_error(
        lmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                          offset = NULL, weights = NULL, dataName = "nonexistent_object", REML = TRUE,
                          control_type = NULL, control_value.transmit = NULL,
                          optimizer = NULL, verbose = 0),
        regexp = "does not exist"
    )
})

test_that("lmerSLMADS.assign throws error when dataName is not a data.frame or matrix", {
    bad_input <- c(1, 2, 3)
    expect_error(
        lmerSLMADS.assign(formula = stats::as.formula("y ~ x + (1|group)"),
                          offset = NULL, weights = NULL, dataName = "bad_input", REML = TRUE,
                          control_type = NULL, control_value.transmit = NULL,
                          optimizer = NULL, verbose = 0),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("lmerSLMADS.assign::smk::shutdown")

# context("lmerSLMADS.assign::smk::done")
