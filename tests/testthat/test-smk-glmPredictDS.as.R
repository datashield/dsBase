#
# Set up
#

# context("glmPredictDS.as::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("glmPredictDS.as::smk::gaussian")
test_that("simple glmPredictDS.as, gaussian, response", {
    D <- data.frame(
        y = c(4.1, 4.2, 3.9, 2.9, 3.6, 3.8, 4.2, 4.2, 2.5, 3.5,
              2.8, 3.7, 3.3, 3.3, 2.4, 3.5, 3.0, 3.1, 3.5, 3.4),
        x = c(41.0, 41.0, 39.0, 38.9, 40.6, 41.0, 40.7, 41.4, 36.0, 39.0,
              37.6, 40.0, 40.3, 40.7, 37.0, 40.0, 41.6, 36.3, 39.3, 40.0)
    )
    my.glm.obj <- glm(y ~ x, family = gaussian(), data = D)

    res <- glmPredictDS.as(glmname.transmit = "my.glm.obj", newdataname.transmit = NULL,
                           output.type = "response", se.fit = FALSE, dispersion = NULL,
                           terms.transmit = NULL, na.action = "na.pass")

    expect_equal(class(res), "list")
    expect_length(res, 1)
    expect_length(res$fit, 20)
    expect_equal(as.numeric(res$fit[1:5]),
                 c(3.7764315943, 3.7764315943, 3.3128909030, 3.2897138684, 3.6837234561),
                 tolerance = 1e-6)
    expect_equal(mean(res$fit), 3.445, tolerance = 1e-6)
})

test_that("glmPredictDS.as throws error when glmname does not exist", {
    expect_error(
        glmPredictDS.as(glmname.transmit = "nonexistent_object", newdataname.transmit = NULL,
                        output.type = "response", se.fit = FALSE, dispersion = NULL,
                        terms.transmit = NULL, na.action = "na.pass"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("glmPredictDS.as::smk::shutdown")

# context("glmPredictDS.as::smk::done")
