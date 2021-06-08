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

  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  thr <- listDisclosureSettingsDS()                           #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################

  input.var <- eval(parse(text=input.var.name), envir = parent.frame())
  factor.levels.present.in.source <- levels(factor(input.var))
  num.levels<-length(factor.levels.present.in.source)
  max.allowed.levels<-length(input.var)*nfilter.levels

  if(num.levels>max.allowed.levels)
  {
	error.message<-
	paste0("FAILED: this variable has too many levels and may be disclosive. The ds.asFactor function allows no more than ",
			max.allowed.levels," levels in this particular study. This variable has ",num.levels)
	stop(error.message, call. = FALSE)
  }

  return(factor.levels.present.in.source)

}
#AGGREGATE FUNCTION
# asFactorDS1
