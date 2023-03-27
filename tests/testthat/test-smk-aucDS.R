#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("aucDS::smk::setup")

#
# Tests
#

test_that("aucDS", {
    set.seed <- 123
    y <- rbinom(n=100, size=1, prob=0.4)
    x1 <- rbinom(n=100, size=1, prob=0.5)
    x2 <- rnorm(n=100, mean=1, sd=0)
    
    mod <- glm(y~x1+x2, family='binomial')
    pred <- predict(mod, response='link')

    res <- aucDS(pred=pred, y=mod$y)

    expect_equal(class(res), "list")
    expect_length(res, 2)
    expect_equal(res$AUC, 0.5191841)
    expect_equal(res$AUC, 0.06353745)
    
})

#
# Done
#

context("aucDS::smk::shutdown")

context("aucDS::smk::done")
