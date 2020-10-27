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

context("lsDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("lsDS::smk::simple")
test_that("simple lsDS", {
    .GlobalEnv$test.obj <- "value"

    search.filter <- NULL
    env.to.search <- 1L

    res <- lsDS(search.filter, env.to.search)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$environment.searched, "R_GlobalEnv")
    expect_equal(class(res$objects.found), "character")
    expect_true("test.obj" %in% res$objects.found)
})

context("lsDS::smk::simple")
test_that("simple lsDS", {
    .GlobalEnv$test.obj <- "value"

    search.filter <- "_:A:_"
    env.to.search <- 1L

    res <- lsDS(search.filter, env.to.search)

    expect_equal(class(res), "list")
    expect_length(res, 3)
    expect_equal(res$environment.searched, "R_GlobalEnv")
    expect_equal(class(res$objects.found), "character")
    expect_true("test.obj" %in% res$objects.found)
    expect_length(res$search.filter.final, 1)
    expect_equal(res$search.filter.final, "*")
})

#
# Done
#

context("lsDS::smk::shutdown")

context("lsDS::smk::done")
