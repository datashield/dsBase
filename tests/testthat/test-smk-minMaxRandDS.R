#-------------------------------------------------------------------------------
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

# context("minMaxRandDS::smk::setup")

#
# Tests
#

test_that("minMaxRandDS", {
    input <- read.csv(file = 'data_files/DASIM/DASIM1.csv')

    res <- minMaxRandDS(input)

    expect_equal(class(res), "numeric")
    expect_length(res, 2)
    expect_true("numeric" %in% class(res[1]))
    expect_true("numeric" %in% class(res[2]))
})

#
# Done
#

# context("minMaxRandDS::smk::shutdown")

# context("minMaxRandDS::smk::done")
