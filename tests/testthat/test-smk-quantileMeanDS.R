#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("quantileMeanDS::smk::setup")

#
# Tests
#

context("quantileMeanDS::smk")
test_that("numeric quantileMeanDS", {
    input <- c(0.0, 1.0, 2.0, 3.0, 4.0)

    res <- quantileMeanDS(input)

    expect_length(res, 8)
    expect_equal(class(res), "numeric")
    expect_equal(res[[1]], 0.2)
    expect_equal(res[[2]], 0.4)
    expect_equal(res[[3]], 1.0)
    expect_equal(res[[4]], 2.0)
    expect_equal(res[[5]], 3.0)
    expect_equal(res[[6]], 3.6)
    expect_equal(res[[7]], 3.8)
    expect_equal(res[[8]], 2.0)

    res.names <- names(res)

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

context("quantileMeanDS::smk::with NA")
test_that("numeric quantileMeanDS, with NA", {
    input <- c(0.0, NA, 2.0, NA, 4.0)

    res <- quantileMeanDS(input)

    expect_length(res, 8)
    expect_equal(class(res), "numeric")
    expect_equal(res[[1]], 0.2)
    expect_equal(res[[2]], 0.4)
    expect_equal(res[[3]], 1.0)
    expect_equal(res[[4]], 2.0)
    expect_equal(res[[5]], 3.0)
    expect_equal(res[[6]], 3.6)
    expect_equal(res[[7]], 3.8)
    expect_equal(res[[8]], 2.0)

    res.names <- names(res)

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

#
# Done
#

context("quantileMeanDS::smk::shutdown")

context("quantileMeanDS::smk::done")
