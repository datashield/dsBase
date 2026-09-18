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

# context("tableDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("tableDS2::smk")
test_that("simple tableDS2", {
    rvar <- factor(rep(c("a", "b"), each = 10))
    cvar <- factor(rep(rep(c("x", "y"), each = 5), 2))
    res.assign <- tableDS.assign("rvar", "cvar", NULL, "a,b", "x,y", NULL, NULL, "no")

    res <- tableDS2("res.assign", "rvar", "cvar", NULL)

    expect_equal(class(res), "list")
    expect_equal(
        names(res),
        c("table.cell.IDs", "table.dim", "table.dimnames", "table.structure_and_cell.order")
    )
    expect_equal(res$table.dim, c(2L, 2L))
    expect_equal(res$table.cell.IDs, 1:4)
})

test_that("tableDS2 throws error when object does not exist", {
    expect_error(
        tableDS2("nonexistent_object", "rvar", "cvar", NULL),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("tableDS2::smk::shutdown")

# context("tableDS2::smk::done")
