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

# context("dataFrameDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("dataFrameDS creates a data.frame from vectors", {
    a <- c(1, 2, 3, 4, 5)
    b <- c(10, 20, 30, 40, 50)

    res <- dataFrameDS("a,b", r.names=NULL, ch.rows=FALSE, ch.names=TRUE,
                       clnames="a,b", strAsFactors=TRUE, completeCases=FALSE)

    expect_equal(class(res), "data.frame")
    expect_equal(nrow(res), 5)
    expect_equal(ncol(res), 2)
    expect_equal(colnames(res), c("a", "b"))
    expect_equal(res$a, a)
    expect_equal(res$b, b)
})

test_that("dataFrameDS handles $ column name syntax", {
    df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(6, 7, 8, 9, 10))

    res <- dataFrameDS("df$x,df$y", r.names=NULL, ch.rows=FALSE, ch.names=TRUE,
                       clnames="df$x,df$y", strAsFactors=TRUE, completeCases=FALSE)

    expect_equal(class(res), "data.frame")
    expect_equal(colnames(res), c("x", "y"))
    expect_equal(res$x, df$x)
    expect_equal(res$y, df$y)
})

test_that("dataFrameDS removes rows with NAs when completeCases is TRUE", {
    a <- c(1, NA, 3, 4, 5)
    b <- c(10, 20, NA, 40, 50)

    res <- dataFrameDS("a,b", r.names=NULL, ch.rows=FALSE, ch.names=TRUE,
                       clnames="a,b", strAsFactors=TRUE, completeCases=TRUE)

    expect_equal(class(res), "data.frame")
    expect_equal(nrow(res), 3)
})

test_that("dataFrameDS errors when object does not exist", {
    expect_error(
        dataFrameDS("nonexistent_obj", r.names=NULL, ch.rows=FALSE, ch.names=TRUE,
                     clnames="nonexistent_obj", strAsFactors=TRUE, completeCases=FALSE),
        regexp = "does not exist"
    )
})

#
# Done
#

# context("dataFrameDS::smk::done")
