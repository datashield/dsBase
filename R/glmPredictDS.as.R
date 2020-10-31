# glmPredictDS.as
#' @title predict regression responses from a glm object
#' @description create a predict_glm object on the serverside by applying
#' the equivalent of predict.glm() in native R to a glm object on the serverside.
#' Identify and return components of the predict_glm object 
#' that can safely be sent to the clientside without a risk of disclosure 
#' @details Serverside assign function called by ds.glmPredict
#' makes predictions of regression responses based on a serverside glm object
#' that has already been created on the serverside by ds.glmSLMA and
#' and writes the predict_glm object to the serverside.
#' For further details see help for ds.glmPredict and help in native R
#' for predict.glm
#' @param glmname.transmit a character string specifying the name of the
#' glm object on the serverside that is to be used for prediction.
#' Fully specified by glmname argument in ds.glmPredict
#' @param newdataname.transmit a character string specifying an (optional) dataframe on
#' the serverside in which to look for (potentially) new covariate values
#' on which to base the predictions. Fully specified by newdataname argument in ds.glmPredict.
#' @param output.type a character string taking the values 'response', 'link' or 'terms'.
#' Fully specified by corresponding argument in ds.glmPredict.
#' @param se.fit logical if standard errors for the fitted predictions are required.
#' Fully specified by corresponding argument in ds.glmPredict.
#' @param dispersion numeric value specifying the dispersion of the GLM fit to be assumed
#' in computing the standard errors. Fully specified by corresponding argument in ds.glmPredict.
#' @param terms.transmit a character vector specifying a subset of terms to return in the
#' prediction. Fully specified by 'terms' argument in ds.glmPredict.
#' @param na.action character string determining what should be done with missing
#' values in the data.frame identified by <newdataname.transmit>.
#' Fully specified by na.action argument in ds.glmPredict.
#' @return glmPredict.as writes a new object to the serverside containing
#' output precisely equivalent to the output from predict.glm in native R.
#' For more details see DataSHIELD help for ds.glmPredict and help for
#' predict.glm in native R
#' @author Paul Burton for DataSHIELD Development Team (20/7/20)
#' @export
glmPredictDS.as <- function(glmname.transmit, newdataname.transmit,
                            output.type,se.fit, dispersion, terms.transmit, na.action){

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


#Check character string denoting <glmname.transmit> argument is not potentially disclosive because of length 

string.safe<-TRUE

if(!is.character(glmname.transmit))
{
  string.safe<-FALSE
}
if(is.character(glmname.transmit))
{
	glmname.text<-strsplit(glmname.transmit, split="")
	string.2.test<-glmname.text
	if(length(string.2.test)>nfilter.stringShort) string.safe<-FALSE
}

if(!string.safe)
{
   studysideMessage<-paste0("FAILED: the argument <glmname> must be a character string no longer than ",
                     "[nfilter.stringShort], i.e. ", nfilter.stringShort," characters")
   return(list(studysideMessage=studysideMessage))
}

#Check character string denoting <newdataname.transmit> argument is not potentially disclosive because of length 

if(!is.null(newdataname.transmit))
{
	string.safe<-TRUE

	if(!is.character(newdataname.transmit))
	{
		string.safe<-FALSE
	}

	if(is.character(newdataname.transmit))
	{
		newdataname.text<-strsplit(newdataname.transmit, split="")
		string.2.test<-newdataname.text
		if(length(string.2.test)>nfilter.stringShort) string.safe<-FALSE
	}

	if(!string.safe)
	{
		studysideMessage<-paste0("FAILED: the argument <newdataname> must be a character string no longer than ",
                     "[nfilter.stringShort], i.e. ", nfilter.stringShort," characters")
	return(list(studysideMessage=studysideMessage))
	}
}

#Check character string denoting <output.type> argument is valid 

string.safe<-TRUE

if(!is.character(output.type))
{
  string.safe<-FALSE
}

if(is.character(output.type))
{
	if((output.type!="link")&&(output.type!="response")&&(output.type!="terms"))
	{
	string.safe<-FALSE
	}
}

if(!string.safe)
{
   studysideMessage<-paste0("FAILED: the argument <output.type> must be one of three character strings: ",
                     "'link','response', or 'terms'")
   return(list(studysideMessage=studysideMessage))
}

#Check character string denoting <na.action> argument is valid
if(!is.character(na.action))
{
  string.safe<-FALSE
} 
if(is.character(na.action))
{
if((na.action!="na.fail")&&(na.action!="na.omit")&&(na.action!="na.exclude")&&(na.action!="na.pass"))
	{
	string.safe<-FALSE
	}
}

if(!string.safe)
{
   studysideMessage<-paste0("FAILED: the argument <na.action> must be one of four character strings: ",
                     "'na.fail','na.omit', 'na.exclude or 'na.pass'")
   return(list(studysideMessage=studysideMessage))
}

#Activate all arguments
#glmobj<-eval(parse(text=glmname.transmit))
glmobj<-get(glmname.transmit)

if(!is.null(newdataname.transmit))
	{
	newdf<-get(newdataname.transmit)
#	newdf<-geeval(parse(text=newdataname.transmit))
	}else{
	newdf<-NULL
	}


#output.type and na.action already OK as character strings

outlist<-stats::predict.glm(object=glmobj,newdata=newdf,type=output.type,
                            se.fit=se.fit,dispersion=dispersion,terms=terms.transmit,na.action=na.action)


if(!se.fit)
  {
  predict.outlist<-list(fit=unlist(outlist))
  }else{
  predict.outlist<-outlist
  }


return(predict.outlist)
}
#ASSIGN FUNCTION
# glmPredictDS.as



