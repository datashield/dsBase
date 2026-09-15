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

# context("cbindDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("cbindDS combines vectors into a data.frame", {
    a <- c(1, 2, 3, 4, 5)
    b <- c(10, 20, 30, 40, 50)

    res <- cbindDS("a,b", "a,b")

    expect_equal(class(res), "data.frame")
    expect_equal(nrow(res), 5)
    expect_equal(ncol(res), 2)
    expect_equal(colnames(res), c("a", "b"))
    expect_equal(res$a, a)
    expect_equal(res$b, b)
})

test_that("cbindDS combines data.frame columns via $ syntax", {
    df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(6, 7, 8, 9, 10))

    res <- cbindDS("df$x,df$y", "df$x,df$y")

    expect_equal(class(res), "data.frame")
    expect_equal(nrow(res), 5)
    expect_equal(ncol(res), 2)
    expect_equal(colnames(res), c("x", "y"))
    expect_equal(res$x, df$x)
    expect_equal(res$y, df$y)
})

test_that("cbindDS makes duplicate column names unique", {
    a <- c(1, 2, 3, 4, 5)
    b <- c(10, 20, 30, 40, 50)

    res <- cbindDS("a,b", "v,v")

    expect_equal(colnames(res), c("v", "v.1"))
})

test_that("cbindDS errors when object does not exist", {
    expect_error(
        cbindDS("nonexistent_obj", "nonexistent_obj"),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("cbindDS::smk::done")
