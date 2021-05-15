#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("mediationTestDS::smk::setup")

#
# Tests
#

context("mediationTestDS::smk::output format")
test_that("check output format mediationTestDS", {
    mv <- c(0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0)
    iv <- c(0.0, 2.0, 1.0, 4.0, 3.0, 0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 2.0, 1.0, 4.0, 3.0, 4.0, 3.0, 0.0, 1.0, 2.0)
    dv <- c(4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 4.0, 3.0, 2.0, 1.0, 0.0, 2.0, 1.0, 0.0, 0.0, 1.0)

    res <- mediationTestDS("mv", "iv", "dv")

    expect_length(res, 3)
    expect_equal(class(res), "data.frame")
    expect_true(all(c('Sobel', 'Aroian', 'Goodman') %in% colnames(res)))
    expect_true(all(c('z.value', 'p.value') %in% rownames(res)))
    expect_length(res$Sobel, 2)
    expect_equal(class(res$Sobel[1]), "numeric")
    expect_equal(res$Sobel[1], -1.3620944, tolerance = 1e-6)
    expect_equal(class(res$Sobel[2]), "numeric")
    expect_equal(res$Sobel[2], 0.1731681, tolerance = 1e-6)
    expect_length(res$Aroian, 2)
    expect_equal(class(res$Aroian[1]), "numeric")
    expect_equal(res$Aroian[1], -1.2898653, tolerance = 1e-6)
    expect_equal(class(res$Aroian[2]), "numeric")
    expect_equal(res$Aroian[2], 0.1970974, tolerance = 1e-6)
    expect_length(res$Goodman, 2)
    expect_equal(class(res$Goodman[1]), "numeric")
    expect_equal(res$Goodman[1], -1.447997, tolerance = 1e-6)
    expect_equal(class(res$Goodman[2]), "numeric")
    expect_equal(res$Goodman[2], 0.147618, tolerance = 1e-6)
})

#
# Done
#

context("mediationTestDS::smk::shutdown")

context("mediationTestDS::smk::done")
