#-------------------------------------------------------------------------------
# Copyright (c) 2024 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

context("metadataDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("metadataDS::smk::list")
test_that("simple metadataDS, list of values", {
    input <- list(v1 = 0.0, v2 = 1.0)

    res <- metadataDS("input")

    expect_equal(class(res), "list")
    expect_length(res, 1)
    expect_length(names(res), 1)
    expect_true("names" %in% names(res))
    expect_equal(class(res$names), "character")
    expect_length(res$names, 2)
    expect_true("v1" %in% res$names)
    expect_true("v2" %in% res$names)
})

context("metadataDS::smk::list field")
test_that("simple metadataDS, list of vectors", {
    input <- list(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))

    res <- metadataDS("input$v1")

    expect_equal(class(res), "list")
    expect_length(res, 0)
})

#
# Done
#

context("metadataDS::smk::shutdown")

context("metadataDS::smk::done")
