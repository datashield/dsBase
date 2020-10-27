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

context("recodeValuesDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple recodeValuesDS1", {
    input          <- c(1, 2, 3, 4, 1, 3)
    values2replace <- "1,3"
    new.values     <- "10,30"

    res <- recodeValuesDS1("input", values2replace, new.values)

    expect_equal(class(res), "character")
    expect_length(res, 1)
    expect_equal(res[1], "Recoding undertaken without problems")
})

#
# Done
#

context("recodeValuesDS1::smk::shutdown")

context("recodeValuesDS1::smk::done")
