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

context("tapplyDS.assign::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("tapplyDS.assign::smk::simple")
test_that("simple tapplyDS.assign", {
    x           <- c(1, 2, 1, 2, 1, 2, 1, 2)
    index       <- factor(c(1, 2, 1, 2, 1, 2, 1, 2))
    INDEX.names <- c('index')
    FUN.name    <- "mean"

    res <- tapplyDS.assign("x", INDEX.names, FUN.name)

    expect_equal(class(res), "list")
    expect_length(res, 1)
    
    res.names <- names(res)

    expect_equal(class(res.names), "character")
    expect_length(res.names, 1)
    expect_equal(res.names[1], "Mean")
    
    expect_equal(class(res$Mean), "array")
    expect_length(res$Mean, 2)
    expect_equal(res$Mean[[1]], 1)
    expect_equal(res$Mean[[2]], 2)
})

#
# Done
#

context("tapplyDS.assign::smk::shutdown")

context("tapplyDS.assign::smk::done")
