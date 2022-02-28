#'
#' @title Determines the levels of the input variable in each single study
#' @description This function is an aggregate DataSHIELD function that returns the
#' levels of the input variable from each single study to the client-side function.
#' @details The function encodes the input vector as factor and returns its levels in
#' ascending order if the levels are numerical or in alphabetical order if the levels
#' are of type character.
#' @param input.var.name the name of the variable that is to be converted to a factor.
#' @return the levels of the input variable.
#' @export
#'
asFactorDS1 <- function(input.var.name=NULL){
  ##################################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                          #
  thr<-dsBase::listDisclosureSettingsDS()                          #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                      #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                      #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)                #
  #nfilter.string <- as.numeric(thr$nfilter.string)                #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)      #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                      #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)                  #
  nfilter.levels.density <- as.numeric(thr$nfilter.levels.density) #
  nfilter.levels.max <- as.numeric(thr$nfilter.levels.max)         #
  ##################################################################

  input.var <- eval(parse(text=input.var.name), envir = parent.frame())
  factor.levels.present.in.source <- levels(factor(input.var))
  num.levels<-length(factor.levels.present.in.source)
  
  max.levels.by.density<-nfilter.levels.density*length(input.var)
  
  if(num.levels>nfilter.levels.max)
  {
    error.message<-
      paste0("FAILED: this variable has too many levels and may be disclosive. It exceeds the max number of levels allowed by nfilter.levels.max: that is ",nfilter.levels.max,". In this study this variable has ",num.levels," factor levels")
    stop(error.message, call. = FALSE)
  }
  
  if(num.levels>(length(input.var)*nfilter.levels.density))
  {
    error.message<-
      paste0("FAILED: this variable has too many levels and may be disclosive. The number of factor levels must not exceed ", (nfilter.levels.density*100), "% of the length of the variable being converted to a factor. The max number of levels in this study is therefore ",max.levels.by.density," but this variable has ",num.levels," factor levels")
    stop(error.message, call. = FALSE)
  }
  
  return(factor.levels.present.in.source)
  
}
#AGGREGATE FUNCTION
# asFactorDS1

