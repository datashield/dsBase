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

# context("tableDS.assign::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("tableDS.assign::smk")
test_that("simple tableDS.assign", {
    rvar <- factor(rep(c("a", "b"), each = 10))
    cvar <- factor(rep(rep(c("x", "y"), each = 5), 2))

    res <- tableDS.assign("rvar", "cvar", NULL, "a,b", "x,y", NULL, NULL, "no")

    expect_equal(class(res), "list")
    expect_equal(names(res), c("table", "counts", "dim", "dimnames"))
    expect_equal(res$dim, c(2L, 2L))
    expect_equal(res$counts, c(5, 5, 5, 5))
})

test_that("tableDS.assign throws error when object does not exist", {
    expect_error(
        tableDS.assign("nonexistent_object", NULL, NULL, "a,b", NULL, NULL, NULL, "no"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("tableDS.assign::smk::shutdown")

# context("tableDS.assign::smk::done")
