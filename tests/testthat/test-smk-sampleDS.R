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

context("sampleDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("sampleDS::smk::simple")
test_that("simple sampleDS", {
    x       <- c(1:32)
    size    <- 16
    replace <- FALSE
    prob    <- NULL

    res <- sampleDS("x", size, replace, prob)

    expect_equal(class(res), "data.frame")
    expect_length(res, 4)

    res.colnames <- colnames(res)
    expect_equal(class(res.colnames), "character")
    expect_equal(res.colnames[1], "x")
    expect_equal(res.colnames[2], "in.sample")
    expect_equal(res.colnames[3], "ID.seq")
    expect_equal(res.colnames[4], "sampling.order")

    expect_length(res$x, 16)
    expect_length(res$in.sample, 16)
    expect_length(res$ID.seq, 16)
    expect_length(res$sampling.order, 16)
})

#
# Done
#

context("sampleDS::smk::shutdown")

context("sampleDS::smk::done")
