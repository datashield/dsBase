# glmSummaryDS.as
#' @title summarize a glm object on the serverside
#' @description summarize a glm object on the serverside to create a 
#' summary_glm object. Also identify and return components of 
#' both the glm object and the summary_glm object 
#' that can safely be sent to the clientside without a risk of disclosure 
#' @details Serverside assign function called by ds.glmSummary
#' summarises a glm object that has already been created on the serverside by
#' fitting ds.glmSLMA and writes the summary_glm object to the serverside.
#' For further details see help for ds.glmSLMA and help in native R
#' for glm() and summary.glm
#' @param x.transmit a character string specifying the name of the
#' glm object on the serverside that is to be summarised. This is
#' specified by x.name argument in ds.glmSummary
#' @return writes object to serverside which is precisely equivalent
#' to summary(glm object) in native R
#' @author Paul Burton for DataSHIELD Development Team (20/7/20)
#' @export

glmSummaryDS.as <- function(x.transmit){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS() 								#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################


#Check character string denoting <x.transmit> argument is not potentially disclosive because of length 

string.safe<-TRUE

if(is.character(x.transmit))
{
	x.text<-strsplit(x.transmit, split="")
	string.2.test<-x.text
	if(length(string.2.test)>nfilter.stringShort) string.safe<-FALSE
}

if(!string.safe)
{
   studysideMessage<-"FAILED: the character string denoting the argument <x.name> is too long and may be disclosive - please shorten"
   return(list(studysideMessage=studysideMessage))
}


#create summary.glm object

input.obj<-eval(parse(text=x.transmit))

summary.obj<-summary(input.obj)

#block na.action and deviance residual components of summary object
summary.obj[[12]]<-NA
summary.obj[[11]]<-NA

summary.obj<-summary.obj

return(summary.obj)

}

#assign function
# glmSummaryDS.as




