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

# context("tableDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("tableDS::smk::1D")
test_that("simple tableDS, 1D", {
    input <- factor(rep(c("a", "b"), each = 5))

    res <- tableDS("input", NULL, NULL, "a,b", NULL, NULL, NULL, "no", NULL)

    expect_equal(class(res), "table")
    expect_equal(as.vector(res), c(5, 5))
    expect_equal(names(res), c("a", "b"))
})

# context("tableDS::smk::2D")
test_that("simple tableDS, 2D", {
    rvar <- factor(rep(c("a", "b"), each = 10))
    cvar <- factor(rep(rep(c("x", "y"), each = 5), 2))

    res <- tableDS("rvar", "cvar", NULL, "a,b", "x,y", NULL, NULL, "no", NULL)

    expect_equal(class(res), "table")
    expect_equal(dim(res), c(2L, 2L))
    expect_equal(as.vector(res), c(5, 5, 5, 5))
})

test_that("tableDS throws error when object does not exist", {
    expect_error(
        tableDS("nonexistent_object", NULL, NULL, "a,b", NULL, NULL, NULL, "no", NULL),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("tableDS::smk::shutdown")

# context("tableDS::smk::done")
