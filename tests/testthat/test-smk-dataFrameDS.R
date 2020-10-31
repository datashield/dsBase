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

context("dataFrameDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("dataFrameDS::smk")
test_that("simple dataFrameDS", {
    v1            <- c(0.0, 1.0, 2.0, 3.0, 4.0)
    v2            <- c(4.0, 3.0, 2.0, 1.0, 0.0)
    vectors       <- "v1,v2"
    r.names       <- NULL
    ch.rows       <- FALSE
    ch.names      <- FALSE
    clnames       <- "x1,x2"
    strAsFactors  <- FALSE
    completeCases <- FALSE

    res <- dataFrameDS(vectors, r.names, ch.rows, ch.names, clnames, strAsFactors, completeCases)

    expect_equal(class(res), "data.frame")
    expect_length(res, 2)

    res.classes <- colnames(res)
    expect_length(res.classes, 2)
    expect_equal(res.classes[1], "x1")
    expect_equal(res.classes[2], "x2")

    for (index in 1:length(res))
    {
        expect_equal(v1[index], res$x1[index], info = paste0('index=', index, ', column=x1'))
        expect_equal(v2[index], res$x2[index], info = paste0('index=', index, ', column=x2'))
    }
})

#
# Stutdown
#

context("dataFrameDS::smk::shutdown")

#
# Done
#

context("dataFrameDS::smk::done")
