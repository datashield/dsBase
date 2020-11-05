#'
#' @title Return the names of a list object
#' @description Returns the names of a designated server-side list
#' @details namesDS is an aggregate function called by ds.names.
#' This function is similar to the native R function \code{names}
#' but it does not subsume all functionality,
#' for example, it only works to extract names that already exist,
#' not to create new names for objects.
#' The function is restricted to objects of type list,
#' but this includes objects that have a primary class other than list but which
#' return TRUE to the native R function {is.list}. As an example, this includes
#' the multi-component object created by fitting a generalized linear model
#' using ds.glmSLMA. The resultant object saved on each separate server
#' is formally of double class "glm" and "ls" but responds TRUE to is.list(),
#' @param xname.transmit a character string specifying the name of the list.
#' @return \code{namesDS} returns to the client-side the names
#' of a list object stored on the server-side.
#' @author Amadou Gaye, updated by Paul Burton 25/06/2020
#' @export
#'
namesDS <- function(xname.transmit){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
thr<-listDisclosureSettingsDS()                                         #
#nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
#nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
#nfilter.string<-as.numeric(thr$nfilter.string)                         #
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)                #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################


  #Check character string denoting <x.transmit> argument is not potentially disclosive because of length

  string.safe<-TRUE

  if(is.character(xname.transmit))
  {
    xname.text<-strsplit(xname.transmit, split="")
    string.2.test<-xname.text
    if(length(string.2.test)>nfilter.stringShort) string.safe<-FALSE
  }

  if(!string.safe)
  {
    studysideMessage<-"FAILED: the character string denoting the argument <xname> is too long and may be disclosive - please shorten"
    return(list(studysideMessage=studysideMessage))
  }

  list.obj<-eval(parse(text=xname.transmit), envir = parent.frame())

  trace.message<-class(list.obj)


  if(!is.list(list.obj)){
    error.message <- "The input object is not of class <list>"
    return(list(error.message=error.message,trace.message=trace.message))
  }


  output <- names(list.obj)

#  outlist<-list(names=output)

  return(output)
}
#AGGREGATE FUNCTION
# namesDS
