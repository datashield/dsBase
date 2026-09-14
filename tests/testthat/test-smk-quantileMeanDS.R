#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

# context("quantileMeanDS::smk::setup")

#
# Tests
#

# context("quantileMeanDS::smk")
test_that("numeric quantileMeanDS", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    res <- quantileMeanDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$class, "numeric")

    qq <- res$quantiles

    expect_length(qq, 8)
    expect_equal(class(qq), "numeric")
    expect_equal(qq[[1]], 0.2)
    expect_equal(qq[[2]], 0.4)
    expect_equal(qq[[3]], 1.0)
    expect_equal(qq[[4]], 2.0)
    expect_equal(qq[[5]], 3.0)
    expect_equal(qq[[6]], 3.6)
    expect_equal(qq[[7]], 3.8)
    expect_equal(qq[[8]], 2.0)

    res.names <- names(qq)

    expect_length(res.names, 8)
    expect_equal(class(res.names), "character")
    expect_equal(res.names[[1]], "5%")
    expect_equal(res.names[[2]], "10%")
    expect_equal(res.names[[3]], "25%")
    expect_equal(res.names[[4]], "50%")
    expect_equal(res.names[[5]], "75%")
    expect_equal(res.names[[6]], "90%")
    expect_equal(res.names[[7]], "95%")
    expect_equal(res.names[[8]], "Mean")
})

# context("quantileMeanDS::smk::with NA")
test_that("numeric quantileMeanDS, with NA", {
    input <- c(0.0, NA, 2.0, NA, 4.0)

    res <- quantileMeanDS("input")

    expect_equal(class(res), "list")
    expect_equal(res$class, "numeric")

    qq <- res$quantiles

    expect_length(qq, 8)
    expect_equal(class(qq), "numeric")
    expect_equal(qq[[1]], 0.2)
    expect_equal(qq[[2]], 0.4)
    expect_equal(qq[[3]], 1.0)
    expect_equal(qq[[4]], 2.0)
    expect_equal(qq[[5]], 3.0)
    expect_equal(qq[[6]], 3.6)
    expect_equal(qq[[7]], 3.8)
    expect_equal(qq[[8]], 2.0)

    res.names <- names(qq)

    expect_length(res.names, 8)
    expect_equal(class(res.names), "character")
    expect_equal(res.names[[1]], "5%")
    expect_equal(res.names[[2]], "10%")
    expect_equal(res.names[[3]], "25%")
    expect_equal(res.names[[4]], "50%")
    expect_equal(res.names[[5]], "75%")
    expect_equal(res.names[[6]], "90%")
    expect_equal(res.names[[7]], "95%")
    expect_equal(res.names[[8]], "Mean")
})

test_that("quantileMeanDS throws error when object does not exist", {
    expect_error(quantileMeanDS("nonexistent_object"), regexp = "does not exist")
})

test_that("quantileMeanDS throws error when object is not numeric or integer", {
    bad_input <- c("a", "b", "c")
    expect_error(quantileMeanDS("bad_input"), regexp = "must be of type numeric or integer")
})

#
# Done
#

# context("quantileMeanDS::smk::shutdown")

# context("quantileMeanDS::smk::done")
