ENV <- new.env()

.onLoad = function(libname, pkgname) {
  
  #### !!! If making changes, update: .onLoad(), set_opts(), show_opts(), .check_options()
  
  options(
    datashield.privacyLevel = 5, 
    default.datashield.privacyControlLevel = "banana", 
    default.nfilter.glm = 0.33, 
    default.nfilter.kNN = 3, 
    default.nfilter.string = 80, 
    default.nfilter.subset = 3, 
    default.nfilter.stringShort = 20, 
    default.nfilter.tab = 3, 
    default.nfilter.noise = 0.25, 
    default.nfilter.levels.density = 0.33, 
    default.nfilter.levels.max = 40
  )
}