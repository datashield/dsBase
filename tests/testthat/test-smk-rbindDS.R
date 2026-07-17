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

# context("rbindDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("rbindDS combines two data.frames by row", {
    df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
    df2 <- data.frame(a = c(7, 8), b = c(9, 10))

    res <- rbindDS("df1,df2", "a,b")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 5)
    expect_equal(ncol(res), 2)
    expect_equal(colnames(res), c("a", "b"))
    expect_equal(res[, "a"], c(1, 2, 3, 7, 8))
    expect_equal(res[, "b"], c(4, 5, 6, 9, 10))
})

test_that("rbindDS combines vectors", {
    v1 <- c(1, 2, 3)
    v2 <- c(4, 5, 6)

    res <- rbindDS("v1,v2", "V1")

    expect_true(is.matrix(res))
    expect_equal(nrow(res), 6)
    expect_equal(ncol(res), 1)
    expect_equal(colnames(res), "V1")
})

test_that("rbindDS errors when object does not exist", {
    expect_error(
        rbindDS("nonexistent_obj", "a"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("rbindDS::smk::done")
