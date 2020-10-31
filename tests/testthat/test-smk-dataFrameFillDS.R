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

context("dataFrameFillDS::smk::setup")

#
# Tests
#

context("dataFrameFillDS::smk")
test_that("simple dataFrameFillDS, ascending, numeric", {
    df                  <- data.frame(v1 = c(-2.0, -3.0, 4.0, 2.0, 1.0, 0.0, -1.0, 3.0), v2 = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
    allNames.transmit   <- "v1,v2,v3,v4,v5,v6,v7"
    class.vect.transmit <- "numeric,numeric,numeric,integer,character,factor,logical"

    res <- dataFrameFillDS("df", allNames.transmit, class.vect.transmit)

    expect_equal(class(res), "data.frame")
    expect_length(res, 7)

    res.colnames <- colnames(res)
    expect_length(res.colnames, 7)
    expect_equal(res.colnames[1], 'v1')
    expect_equal(res.colnames[2], 'v2')
    expect_equal(res.colnames[3], 'v3')
    expect_equal(res.colnames[4], 'v4')
    expect_equal(res.colnames[5], 'v5')
    expect_equal(res.colnames[6], 'v6')
    expect_equal(res.colnames[7], 'v7')

    expect_length(res$v1, 8)
    expect_length(res$v2, 8)
    expect_length(res$v3, 8)
    expect_length(res$v4, 8)
    expect_length(res$v5, 8)
    expect_length(res$v6, 8)
    expect_length(res$v7, 8)

    expect_equal(class(res$v1), "numeric")
    expect_equal(class(res$v2), "numeric")
    expect_equal(class(res$v3), "numeric")
    expect_equal(class(res$v4), "integer")
    expect_equal(class(res$v5), "character")
    expect_equal(class(res$v6), "factor")
    expect_equal(class(res$v7), "logical")

    expect_equal(res$v1[1], -2.0)
    expect_equal(res$v2[1], 0.0)
    expect_equal(res$v1[2], -3.0)
    expect_equal(res$v2[2], 1.0)
    expect_equal(res$v1[3], 4.0)
    expect_equal(res$v2[3], 2.0)
    expect_equal(res$v1[4], 2.0)
    expect_equal(res$v2[4], 3.0)
    expect_equal(res$v1[5], 1.0)
    expect_equal(res$v2[5], 4.0)
    expect_equal(res$v1[6], 0.0)
    expect_equal(res$v2[6], 5.0)
    expect_equal(res$v1[7], -1.0)
    expect_equal(res$v2[7], 6.0)
    expect_equal(res$v1[8], 3.0)
    expect_equal(res$v2[8], 7.0)

    for (index in 1:8)
    {
        expect_true(is.na(res$v3[index]), info = paste0('index=', index, ', column=v3'))
        expect_true(is.na(res$v4[index]), info = paste0('index=', index, ', column=v4'))
        expect_true(is.na(res$v5[index]), info = paste0('index=', index, ', column=v5'))
        expect_true(is.na(res$v6[index]), info = paste0('index=', index, ', column=v6'))
        expect_true(is.na(res$v7[index]), info = paste0('index=', index, ', column=v7'))
    }
})

#
# Shutdown
#

context("dataFrameFillDS::smk::shutdown")

#
# Done
#

context("dataFrameFillDS::smk::done")
