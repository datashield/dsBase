#
# Set up
#

# context("lmerSLMA2DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lmerSLMA2DS::smk::random intercept")
test_that("simple lmerSLMA2DS, random intercept", {
    set.seed(42)
    D <- data.frame(
        group = factor(rep(1:4, each = 6)),
        x = rep(1:6, times = 4)
    )
    D$y <- 2 + 0.5 * D$x + rep(rnorm(4, sd = 1), each = 6) + rnorm(24, sd = 0.3)

    res <- suppressWarnings(
        lmerSLMA2DS(formula = stats::as.formula("y ~ x + (1|group)"),
                   offset = NULL, weights = NULL, dataName = "D", REML = TRUE,
                   control_type = NULL, control_value.transmit = NULL,
                   optimizer = NULL, verbose = 0)
    )

    expect_equal(class(res), "summary.merMod")
    expect_equal(res$errorMessage, "No errors")
    expect_equal(res$disclosure.risk, 0)
    expect_equal(res$Ntotal, 24)
    expect_equal(res$Nvalid, 24)
    expect_equal(res$Nmissing, 0)
    expect_equal(dim(res$coefficients), c(2, 3))
    expect_equal(res$coefficients[, "Estimate"],
                 c(`(Intercept)` = 2.5942648, x = 0.4604968), tolerance = 1e-5)
    expect_equal(res$sigma, 0.3874094, tolerance = 1e-5)
})

test_that("lmerSLMA2DS throws error when dataName does not exist", {
    expect_error(
        lmerSLMA2DS(formula = stats::as.formula("y ~ x + (1|group)"),
                   offset = NULL, weights = NULL, dataName = "nonexistent_object", REML = TRUE,
                   control_type = NULL, control_value.transmit = NULL,
                   optimizer = NULL, verbose = 0),
        regexp = "does not exist"
    )
})

test_that("lmerSLMA2DS throws error when dataName is not a data.frame or list", {
    bad_input <- c(1, 2, 3)
    expect_error(
        lmerSLMA2DS(formula = stats::as.formula("y ~ x + (1|group)"),
                   offset = NULL, weights = NULL, dataName = "bad_input", REML = TRUE,
                   control_type = NULL, control_value.transmit = NULL,
                   optimizer = NULL, verbose = 0),
        regexp = "must be of type"
    )
})

test_that("lmerSLMA2DS gives the same result for data as a list as for a data.frame", {
    set.seed(42)
    D <- data.frame(
        group = factor(rep(1:4, each = 6)),
        x = rep(1:6, times = 4)
    )
    D$y <- 2 + 0.5 * D$x + rep(rnorm(4, sd = 1), each = 6) + rnorm(24, sd = 0.3)
    D.list <- as.list(D)

    res.df <- suppressWarnings(lmerSLMA2DS(formula = stats::as.formula("y ~ x + (1|group)"),
        offset = NULL, weights = NULL, dataName = "D", REML = TRUE,
        control_type = NULL, control_value.transmit = NULL, optimizer = NULL, verbose = 0))
    res.list <- suppressWarnings(lmerSLMA2DS(formula = stats::as.formula("y ~ x + (1|group)"),
        offset = NULL, weights = NULL, dataName = "D.list", REML = TRUE,
        control_type = NULL, control_value.transmit = NULL, optimizer = NULL, verbose = 0))

    expect_equal(res.list, res.df)
})

#
# Done
#

# context("lmerSLMA2DS::smk::shutdown")

# context("lmerSLMA2DS::smk::done")
