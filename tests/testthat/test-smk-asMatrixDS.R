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

context("asMatrixDS::smk::setup")

#
# Tests
#

context("asMatrixDS::smk::simple")
test_that("simple asMatrixDS", {
    input <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- asMatrixDS("input")

    res.class <- class(res)
    if (base::getRversion() < 4.0)
    {
        expect_length(res.class, 1)
        expect_true("matrix" %in% res.class)
    }
    else
    {
        expect_length(res.class, 2)
        expect_true("matrix" %in% res.class)
        expect_true("array" %in% res.class)
    }

    expect_length(res, 10)
    expect_equal(res[1], 0)
    expect_equal(res[2], 1)
    expect_equal(res[3], 2)
    expect_equal(res[4], 3)
    expect_equal(res[5], 4)
    expect_equal(res[6], 4)
    expect_equal(res[7], 3)
    expect_equal(res[8], 2)
    expect_equal(res[9], 1)
    expect_equal(res[10], 0)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 2)    
    expect_equal(res.colnames[1], "v1")
    expect_equal(res.colnames[2], "v2")
})

#
# Done
#

context("asMatrixDS::smk::shutdown")

context("asMatrixDS::smk::done")
