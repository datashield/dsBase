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

# context("repDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("repDS with serverside scalar x1", {
    myScalar <- 5

    res <- repDS(x1.transmit="myScalar", times.transmit="3", length.out.transmit=NULL,
                 each.transmit=NULL, x1.includes.characters=FALSE,
                 source.x1="serverside", source.times="clientside",
                 source.length.out=NULL, source.each=NULL)

    expect_equal(res, c(5, 5, 5))
})

test_that("repDS with serverside vector x1", {
    myVec <- c(1, 2, 3)

    res <- repDS(x1.transmit="myVec", times.transmit="2", length.out.transmit=NULL,
                 each.transmit=NULL, x1.includes.characters=FALSE,
                 source.x1="serverside", source.times="clientside",
                 source.length.out=NULL, source.each=NULL)

    expect_equal(res, c(1, 2, 3, 1, 2, 3))
})

test_that("repDS with clientside x1", {
    res <- repDS(x1.transmit="7", times.transmit="4", length.out.transmit=NULL,
                 each.transmit=NULL, x1.includes.characters=FALSE,
                 source.x1="clientside", source.times="clientside",
                 source.length.out=NULL, source.each=NULL)

    expect_equal(res, c(7, 7, 7, 7))
})

test_that("repDS errors when serverside x1 object does not exist", {
    expect_error(
        repDS(x1.transmit="nonexistent_obj", times.transmit="3", length.out.transmit=NULL,
              each.transmit=NULL, x1.includes.characters=FALSE,
              source.x1="serverside", source.times="clientside",
              source.length.out=NULL, source.each=NULL),
        regexp = "does not exist"
    )
})

test_that("repDS errors when serverside times object does not exist", {
    myScalar <- 5

    expect_error(
        repDS(x1.transmit="myScalar", times.transmit="nonexistent_times", length.out.transmit=NULL,
              each.transmit=NULL, x1.includes.characters=FALSE,
              source.x1="serverside", source.times="serverside",
              source.length.out=NULL, source.each=NULL),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("repDS::smk::shutdown")

# context("repDS::smk::done")
