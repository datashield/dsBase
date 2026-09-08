#
# Set up
#

# context("glmPredictDS.ag::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmPredictDS.ag::smk::gaussian")
test_that("simple glmPredictDS.ag, gaussian, response", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )
    my.glm.obj <- glm(y ~ x, family = gaussian(), data = D)

    res <- glmPredictDS.ag(glmname.transmit = "my.glm.obj", newdataname.transmit = NULL,
                           output.type = "response", se.fit = FALSE, dispersion = NULL,
                           terms.transmit = NULL, na.action = "na.pass")

    expect_equal(class(res), "list")
    expect_length(res, 1)
    sl <- res$safe.list
    expect_equal(sl$glm.object, "my.glm.obj")
    expect_null(sl$newdfname)
    expect_equal(sl$output.type, "response")
    expect_equal(sl$fit.Ntotal, 20)
    expect_equal(sl$fit.Nvalid, 20)
    expect_equal(sl$fit.Nmiss, 0)
    expect_equal(sl$fit.mean, 3.445, tolerance = 1e-6)
    expect_equal(sl$fit.sd, 0.6233470588, tolerance = 1e-6)
    expect_equal(
        as.numeric(sl$fit.quantiles),
        c(2.6836344144, 2.8331262874, 3.3070966443, 3.5446612486,
          3.7242832665, 3.7857024081, 3.8714574361),
        tolerance = 1e-6
    )
})

test_that("glmPredictDS.ag throws error when glmname does not exist", {
    expect_error(
        glmPredictDS.ag(glmname.transmit = "nonexistent_object", newdataname.transmit = NULL,
                        output.type = "response", se.fit = FALSE, dispersion = NULL,
                        terms.transmit = NULL, na.action = "na.pass"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("glmPredictDS.ag::smk::shutdown")

# context("glmPredictDS.ag::smk::done")
