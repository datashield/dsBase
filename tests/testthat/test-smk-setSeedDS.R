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

context("setSeedDS::smk::setup")

#
# Tests
#

context("setSeedDS::smk::simple")
test_that("simple setSeedDS", {
    seedtext    <- "19"
    kind        <- NULL
    normal.kind <- NULL

    res <- setSeedDS(seedtext, kind, normal.kind)

    expect_equal(class(res), "list")
    expect_length(res, 1)
    expect_length(res$seed.as.set, 626)
})

#
# Done
#

context("setSeedDS::smk::shutdown")

context("setSeedDS::smk::done")
