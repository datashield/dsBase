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

# context("reShapeDS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

test_that("simple reShapeDS wide to long", {
    wide_df <- data.frame(
        id    = 1:3,
        sbp.1 = c(120, 130, 125),
        sbp.2 = c(122, 135, 128)
    )

    res <- reShapeDS(
        data.name         = "wide_df",
        varying.transmit  = "sbp.1,sbp.2",
        v.names.transmit  = "sbp",
        timevar.name      = "time",
        idvar.name        = "id",
        drop.transmit     = NULL,
        direction         = "long",
        sep               = "."
    )

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 6)
    expect_true("sbp" %in% colnames(res))
    expect_true("time" %in% colnames(res))
})
