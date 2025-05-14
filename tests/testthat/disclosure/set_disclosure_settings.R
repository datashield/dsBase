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

set.specific.disclosure.settings <- function(datashield.privacyControlLevel='permissive',
                                             nfilter.tab='3',
                                             nfilter.subset='3',
                                             nfilter.glm='0.33',
                                             nfilter.string='80',
                                             nfilter.stringShort='20',
                                             nfilter.kNN='3',
                                             nfilter.levels.density='0.33',
                                             nfilter.levels.max='40',
                                             nfilter.noise='0.25',
                                             nfilter.privacy.old='5') {
  options(datashield.privacyLevel = nfilter.privacy.old)
  options(default.datashield.privacyControlLevel = datashield.privacyControlLevel)
  options(default.nfilter.glm = nfilter.glm)
  options(default.nfilter.kNN = nfilter.kNN)
  options(default.nfilter.string = nfilter.string)
  options(default.nfilter.subset = nfilter.subset)
  options(default.nfilter.stringShort = nfilter.stringShort)
  options(default.nfilter.tab = nfilter.tab)
  options(default.nfilter.noise = nfilter.noise)
  options(default.nfilter.levels.density = nfilter.levels.density)
  options(default.nfilter.levels.max = nfilter.levels.max)
}
