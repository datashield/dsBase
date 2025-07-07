#' 
#' @title lexisDS1
#'
#' @description The first server-side function called by ds.lexis.
#' @details This is an aggregate function.
#' For more details see the extensive header for ds.lexis.
#' @param exitCol a character string specifying the variable holding the time that each individual is censored or fails
#' 
#' @author Burton PR
#' 
#' @return List with `max.time`
#' @export
#'
lexisDS1 <- function(exitCol=NULL){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  #nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  exitCol.length <- length(strsplit(exitCol,"")[[1]])
  if(exitCol.length>nfilter.string){
    errorMessage <- "ERROR: character string naming exitCol is too long please shorten name"
    stop(errorMessage, call. = FALSE)
  }
  
  exposure <- eval(parse(text=exitCol), envir = parent.frame())
  
  max.time <- max(exposure, na.rm=TRUE)
  random.multiplier <- stats::runif(1,1.01,1.05)
  
  max.time <- max.time*random.multiplier
  
  out.obj <- list(max.time=max.time)
  
  return(out.obj)
  
}
#AGGREGATE FUNCTION
# lexisDS1
