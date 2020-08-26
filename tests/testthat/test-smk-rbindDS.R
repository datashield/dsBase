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

context("rbindDS::smk::setup")

#
# Tests
#

context("rbindDS::smk::simple")
test_that("simple rbindDS", {
    inputs   <- 'input1, input2'
    input1   <- c(0.0, 1.0, 2.0, 3.0)
    input2   <- c(3.0, 2.0, 1.0, 0.0)
    colnames <- 'v1'

    res <- rbindDS(inputs, colnames)

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

    expect_length(res, 8)
    expect_equal(class(res[1]), "numeric")
    expect_equal(class(res[2]), "numeric")
    expect_equal(class(res[3]), "numeric")
    expect_equal(class(res[4]), "numeric")
    expect_equal(class(res[5]), "numeric")
    expect_equal(class(res[6]), "numeric")
    expect_equal(class(res[7]), "numeric")
    expect_equal(class(res[8]), "numeric")

    res.colnames <- colnames(res)

    expect_equal(class(res.colnames), "character")
    expect_length(res.colnames, 1)
    expect_equal(res.colnames[1], 'v1')
})

#
# Done
#

context("rbindDS::smk::shutdown")

context("rbindDS::smk::done")
