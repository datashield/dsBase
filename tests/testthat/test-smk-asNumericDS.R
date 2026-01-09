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

# context("asNumericDS::smk::setup")

#
# Tests
#

# context("asNumericDS::smk::character")
test_that("character asNumericDS - FALSE", {
    input <- "101"

    res <- asNumericDS("input")

    expect_length(res, 1)
    expect_equal(class(res), "numeric")
    expect_equal(res, 101)
})

# context("asNumericDS::smk::character vector")
test_that("character vector asNumericDS", {
    input <- c("101", "202", "303", "404", "505")

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 101)
    expect_equal(res[2], 202)
    expect_equal(res[3], 303)
    expect_equal(res[4], 404)
    expect_equal(res[5], 505)
})

# context("asNumericDS::smk::character 'non numeric' vector")
test_that("character 'non numeric' vector asNumericDS", {
    input <- c("aa", "bb", "cc", "dd", "ee")

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 2)
    expect_equal(res[3], 3)
    expect_equal(res[4], 4)
    expect_equal(res[5], 5)
})

# context("asNumericDS::smk::factor vector")
test_that("factor vector asNumericDS", {
    vec   <- c("101", "202", "303", "404", "505")
    input <- as.factor(vec)

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 101)
    expect_equal(res[2], 202)
    expect_equal(res[3], 303)
    expect_equal(res[4], 404)
    expect_equal(res[5], 505)
})

# context("asNumericDS::smk::factor rev vector")
test_that("factor vector asNumericDS", {
    vec   <- c("505", "404", "303", "202", "101")
    input <- as.factor(vec)

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 505)
    expect_equal(res[2], 404)
    expect_equal(res[3], 303)
    expect_equal(res[4], 202)
    expect_equal(res[5], 101)
})

# context("asNumericDS::smk::factor numeric levels vector")
test_that("factor numeric levels vector asNumericDS", {
    vec           <- c("aa", "bb", "cc", "dd", "ee")
    input         <- as.factor(vec)
    levels(input) <- c("11", "22", "33", "44", "55") 

    res <- asNumericDS("input")

    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 11)
    expect_equal(res[2], 22)
    expect_equal(res[3], 33)
    expect_equal(res[4], 44)
    expect_equal(res[5], 55)
})

# context("asNumericDS::smk::factor vector with only numbers in its values")
test_that("factor vector with only numbers in its values asNumericDS", {
    input <- as.factor(c('1','1','2','2','1')) 
    
    res <- asNumericDS("input")
    
    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 2)
    expect_equal(res[4], 2)
    expect_equal(res[5], 1)
})

# context("asNumericDS::smk::factor vector with only characters in its values")
test_that("factor vector with only characters in its values asNumericDS", {
    input <- as.factor(c('b','b','a','a','b')) 
    
    res <- asNumericDS("input")
    
    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 2)
    expect_equal(res[2], 2)
    expect_equal(res[3], 1)
    expect_equal(res[4], 1)
    expect_equal(res[5], 2)
})

# context("asNumericDS::smk::character vector with only numbers in its values")
test_that("factor vector with only numbers in its values asNumericDS", {
    input <- c('1','1','2','2','1')
    
    res <- asNumericDS("input")
    
    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 2)
    expect_equal(res[4], 2)
    expect_equal(res[5], 1)
})

# context("asNumericDS::smk::character vector with only characters in its values")
test_that("character vector with only characters in its values asNumericDS", {
    input <- c('b','b','a','a','b')
    
    res <- asNumericDS("input")
    
    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 2)
    expect_equal(res[2], 2)
    expect_equal(res[3], 1)
    expect_equal(res[4], 1)
    expect_equal(res[5], 2)
})

# context("asNumericDS::smk::character vector with strings having characters and numbers")
test_that("character vector with strings having characters and numbers asNumericDS", {
    input <- c('b1','b2','1a','a','b')
    
    res <- asNumericDS("input")
    
    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 4)
    expect_equal(res[2], 5)
    expect_equal(res[3], 1)
    expect_equal(res[4], 2)
    expect_equal(res[5], 3)
})

# context("asNumericDS::smk::logical vector")
test_that("logical vector asNumericDS", {
    input <- c(TRUE, TRUE, FALSE, TRUE)
    
    res <- asNumericDS("input")
    
    expect_length(res, 4)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 0)
    expect_equal(res[4], 1)
})

# context("asNumericDS::smk::logical character vector")
test_that("logical vector character asNumericDS", {
    input <- c("TRUE", "TRUE", "FALSE", "TRUE")
    
    res <- asNumericDS("input")
    
    expect_length(res, 4)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 2)
    expect_equal(res[2], 2)
    expect_equal(res[3], 1)
    expect_equal(res[4], 2)
})

# context("asNumericDS::smk::integer vector")
test_that("integer vector asNumericDS", {
    input <- as.integer(c('1','1','2','2','1')) 
    
    res <- asNumericDS("input")
    
    expect_length(res, 5)
    expect_equal(class(res), "numeric")
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 2)
    expect_equal(res[4], 2)
    expect_equal(res[5], 1)
})

#
# Done
#

# context("asNumericDS::smk::shutdown")

# context("asNumericDS::smk::done")
