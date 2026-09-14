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

# context("mergeDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("mergeDS happy path, simple merge", {
    x.df <- data.frame(id = c(1, 2, 3), val_x = c("a", "b", "c"))
    y.df <- data.frame(id = c(1, 2, 3), val_y = c("d", "e", "f"))

    res <- mergeDS("x.df", "y.df", "id", "id", FALSE, FALSE, TRUE, ".x,.y", TRUE, NULL)

    expect_true(is.data.frame(res))
    expect_true("val_x" %in% colnames(res))
    expect_true("val_y" %in% colnames(res))
    expect_equal(nrow(res), 3)
})

test_that("mergeDS errors when x.name object does not exist", {
    y.df <- data.frame(id = c(1, 2, 3), val_y = c("d", "e", "f"))

    expect_error(
        mergeDS("nonexistent_object", "y.df", "id", "id", FALSE, FALSE, TRUE, ".x,.y", TRUE, NULL),
        regexp = "does not exist"
    )
})

test_that("mergeDS errors when y.name object does not exist", {
    x.df <- data.frame(id = c(1, 2, 3), val_x = c("a", "b", "c"))

    expect_error(
        mergeDS("x.df", "nonexistent_object", "id", "id", FALSE, FALSE, TRUE, ".x,.y", TRUE, NULL),
        regexp = "does not exist"
    )
})

test_that("mergeDS errors when x.name is not a data.frame", {
    x.df <- c(1, 2, 3)
    y.df <- data.frame(id = c(1, 2, 3), val_y = c("d", "e", "f"))

    expect_error(
        mergeDS("x.df", "y.df", "id", "id", FALSE, FALSE, TRUE, ".x,.y", TRUE, NULL),
        regexp = "must be of type"
    )
})

test_that("mergeDS errors when y.name is not a data.frame", {
    x.df <- data.frame(id = c(1, 2, 3), val_x = c("a", "b", "c"))
    y.df <- c(1, 2, 3)

    expect_error(
        mergeDS("x.df", "y.df", "id", "id", FALSE, FALSE, TRUE, ".x,.y", TRUE, NULL),
        regexp = "must be of type"
    )
})

#
# Done
#

# context("mergeDS::smk::shutdown")

# context("mergeDS::smk::done")
