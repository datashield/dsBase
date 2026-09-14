#
# Set up
#

# context("lmerSLMADS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lmerSLMADS2::smk::random intercept")
test_that("simple lmerSLMADS2, random intercept", {
    set.seed(42)
    D <- data.frame(
        group = factor(rep(1:4, each = 6)),
        x = rep(1:6, times = 4)
    )
    D$y <- 2 + 0.5 * D$x + rep(rnorm(4, sd = 1), each = 6) + rnorm(24, sd = 0.3)

    res <- suppressWarnings(
        lmerSLMADS2(formula = stats::as.formula("y ~ x + (1|group)"),
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

test_that("lmerSLMADS2 throws error when dataName does not exist", {
    expect_error(
        lmerSLMADS2(formula = stats::as.formula("y ~ x + (1|group)"),
                   offset = NULL, weights = NULL, dataName = "nonexistent_object", REML = TRUE,
                   control_type = NULL, control_value.transmit = NULL,
                   optimizer = NULL, verbose = 0),
        regexp = "does not exist"
    )
})

test_that("lmerSLMADS2 throws error when dataName is not a data.frame or matrix", {
    bad_input <- c(1, 2, 3)
    expect_error(
        lmerSLMADS2(formula = stats::as.formula("y ~ x + (1|group)"),
                   offset = NULL, weights = NULL, dataName = "bad_input", REML = TRUE,
                   control_type = NULL, control_value.transmit = NULL,
                   optimizer = NULL, verbose = 0),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("lmerSLMADS2::smk::shutdown")

# context("lmerSLMADS2::smk::done")
