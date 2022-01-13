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
# DataSHIELD disclosure settings
#

set.standard.disclosure.settings <- function() {
    options(datashield.privacyLevel = "5")
    options(default.datashield.privacyControlLevel = "permissive")
    options(default.nfilter.glm = "0.33")
    options(default.nfilter.kNN = "3")
    options(default.nfilter.string = "80")
    options(default.nfilter.subset = "3")
    options(default.nfilter.stringShort = "20")
    options(default.nfilter.tab = "3")
    options(default.nfilter.noise = "0.25")
    options(default.nfilter.levels.density = "0.33")
    options(default.nfilter.levels.max = "40")
}
