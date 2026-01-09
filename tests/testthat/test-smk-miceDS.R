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

# context("miceDS::smk::setup")

#
# Tests
#

# context("miceDS::smk")
test_that("miceDS", {
  
    load(file = 'data_files/CNSIM/CNSIM1.rda')
    D <- study1

    expect_warning(res <- miceDS(data='D', m=1, maxit=5, method=NULL, post=NULL, predictorMatrix=NULL, seed=NA,
                  ncol.pred.mat=NULL, newobj_mids='mids_object', newobj_df='impSet'), "Number of logged events: 1")

    expect_true(all(class(res) %in% c("list")))
    expect_length(res, 3)
    expect_true(all(class(res$method) %in% c("character")))
    expect_equal(as.character(res$method), c("pmm","pmm","pmm","pmm","pmm","","","","","","polyreg"))
    expect_true(all(class(res$predictorMatrix) %in% c("matrix", "array")))
    expect_true(all(class(res$predictorMatrix) %in% c("matrix", "array")))
    expect_equal(as.numeric(res$predictorMatrix[,1]), c(0,1,1,1,1,1,1,1,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,2]), c(1,0,1,1,1,1,1,1,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,3]), c(1,1,0,1,1,1,1,1,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,4]), c(1,1,1,0,1,1,1,1,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,5]), c(1,1,1,1,0,1,1,1,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,6]), c(0,0,0,0,0,0,0,0,0,0,0))
    expect_equal(as.numeric(res$predictorMatrix[,7]), c(1,1,1,1,1,1,0,1,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,8]), c(1,1,1,1,1,1,1,0,1,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,9]), c(1,1,1,1,1,1,1,1,0,1,1))
    expect_equal(as.numeric(res$predictorMatrix[,10]), c(1,1,1,1,1,1,1,1,1,0,1))
    expect_equal(as.numeric(res$predictorMatrix[,11]), c(1,1,1,1,1,1,1,1,1,1,0))
    expect_true("character" %in% class(res$post))
    expect_equal(as.character(res$post), c("","","","","","","","","","",""))
    
})

#
# Done
#

# context("miceDS::smk::shutdown")

# context("miceDS::smk::done")
