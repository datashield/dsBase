# glmSummaryDS.ag
#' @title summarize a glm object on the serverside
#' @description returns the non-disclosive elements to the clientside of a 
#' glm object and the corresponding object holding the output
#' of summary(glm object) on the serverside.
#' @details Serverside aggregate function called by ds.glmSummary.
#' ds.glmSummary first calls glmSummaryDS.ag to create a glm_summary
#' object on the serverside based on applying native R's summary.glm() to a  
#' serverside glm object previously created by ds.glmSLMA. Then it calls
#' glmSummaryDS.ag to return to the clientside all of the non-disclosive
#' elements (and only the non-disclosive elements) of the serverside glm
#' and its corresponding summary_glm object.
#' @param x.transmit a character string specifying the name of the
#' glm object on the serverside that is to be summarised. This is
#' specified by x.name argument in ds.glmSummary
#' @return returns to the clientside all of the non-disclosive
#' elements (and only the non-disclosive elements) of a specified serverside glm
#' and its corresponding summary_glm object.
#' @author Paul Burton for DataSHIELD Development Team (20/7/20)
#' @export

glmSummaryDS.ag <- function(x.transmit){

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

#######################################################################
#create safe glm object with disclosive elements deleted for clientside
#######################################################################

input.obj<-eval(parse(text=x.transmit))


if (is.null(input.obj)) {
    stop('The specified glm object does not exist', call. = FALSE)
}

input.obj.class <- class(input.obj)
if ((! ('glm' %in% input.obj.class)) || (! ('lm' %in% input.obj.class))) {
    stop('The specified glm object is not of class "glm" and "lm"', call. = FALSE)
}

#block potentially disclosive components of input glm object
input.obj$residuals<-NA
input.obj$fitted.values<-NA
input.obj$effects<-NA
input.obj$R<-NA
input.obj$qr<-NA
input.obj$linear.predictors<-NA
input.obj$weights<-NA
input.obj$prior.weights<-NA
input.obj$y<-NA
input.obj$model<-NA
input.obj$na.action<-NA
input.obj$x<-NA
data.cnames<-colnames(input.obj$data)
input.obj$data<-data.cnames
input.obj$offset<-NA

glm.obj<-input.obj

###############################################################################
#create safe summary.glm object with disclosive elements deleted for clientside
###############################################################################

input.obj<-eval(parse(text=x.transmit))

summary.obj<-summary(input.obj)

#block na.action and deviance residual components of summary object
summary.obj$na.action<-NA
summary.obj$deviance.resid<-NA


glm.summary.obj<-summary.obj


outlist<-list(glm.obj=glm.obj,glm.summary.obj=glm.summary.obj)

return(outlist)

}

#aggregate function
# glmSummaryDS.ag




