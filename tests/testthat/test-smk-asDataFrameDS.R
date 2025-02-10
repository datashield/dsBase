#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("asDataFrameDS::smk::setup")

#
# Tests
#

context("asDataFrameDS::smk::simple")
test_that("simple asDataFrameDS", {
    input <- tibble(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- asDataFrameDS("input")

    res.class <- class(res)
    expect_length(res.class, 1)
    expect_true("data.frame" %in% res.class)
    expect_equal(
      dim(res), c(5, 2)
      )
    
    expect_equal(res$v1, 0:4)
    expect_equal(res$v2, 4:0)
    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)    
    expect_equal(res.colnames[1], "v1")
    expect_equal(res.colnames[2], "v2")
})

#
# Done
#

context("asDataMatrixDS::smk::shutdown")
context("asDataMatrixDS::smk::done")
