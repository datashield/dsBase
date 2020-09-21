# glmPredictDS.ag
#' @title predict regression responses from a glm object
#' @description identify and return key components/summaries of a serverside
#' glm_predict object that can safely be returned to the clientside without
#' disclosure risk
#' @details Serverside aggregate function called by ds.glmPredict. It is called
#' immediately after the assign function glmPredict.as has created
#' a predict_glm object on the serverside by applying
#' the equivalent of predict.glm() in native R to a glm object on the serverside.
#' The aggregate function, glmPredict.ag, then identifies and returns components
#' of that predict_glm object that can safely be returned to the clientside
#' without a risk of disclosure. For further details see DataSHIELD help for
#' ds.glmPredict and glmPredict.as and help in native R for predict.glm
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
#' @return components/summarising statistics of a serverside predict_glm object
#' that can safely be transmitted to the clientside
#' without a risk of disclosure. For further details see DataSHIELD help for
#' ds.glmPredict and glmPredict.as and help in native R for predict.glm
#' predict.glm in native R
#' @author Paul Burton for DataSHIELD Development Team (20/7/20)
#' @export
#' 
glmPredictDS.ag <- function(glmname.transmit, newdataname.transmit,
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

#AGGREGATE FUNCTION
# glmPredictDS.ag


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


#prevent non-list output if output is complex (ie more than a single fit of class numeric)

if(output.type=="terms" && se.fit==FALSE)
{
  se.fit<-TRUE
}

outlist<-stats::predict.glm(object=glmobj,newdata=newdf,type=output.type,
                            se.fit=se.fit,dispersion=dispersion,terms=terms.transmit,na.action=na.action)

#ONLY VECTOR OF FITTED VALUES CREATED (LIST OF LENGTH 1)
if(is.numeric(outlist))
  {
  predict.outlist<-list(fit=unlist(outlist))
  
  fit.ag.return.Ntotal<-length(predict.outlist$fit)  
  fit.ag.return.Nvalid<-sum(!is.na(predict.outlist$fit))  
  fit.ag.return.Nmiss<-fit.ag.return.Ntotal-fit.ag.return.Nvalid
  fit.ag.return.mean<-mean(predict.outlist$fit,na.rm=TRUE)
  fit.ag.return.sd<-sqrt(stats::sd(predict.outlist$fit,na.rm=TRUE))  
  fit.ag.return.quantiles<-stats::quantile(predict.outlist$fit,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)


  safe.list<-list(glm.object=glmname.transmit,newdfname=newdataname.transmit,output.type=output.type,
				  dispersion=dispersion,
				  fit.Ntotal=fit.ag.return.Ntotal,fit.Nvalid=fit.ag.return.Nvalid,fit.Nmiss=fit.ag.return.Nmiss,
				  fit.mean=fit.ag.return.mean,fit.sd=fit.ag.return.sd,fit.quantiles=fit.ag.return.quantiles)				  
  return(list(safe.list=safe.list))

 }else{
#MATRIX OF FITTED VALUES, SE etc CREATED (LIST OF LENGTH >1)
 predict.outlist<-outlist
 
  if(output.type!="terms")
  {

	fit.ag.return.Ntotal<-length(predict.outlist$fit)  
	fit.ag.return.Nvalid<-sum(!is.na(predict.outlist$fit))  
	fit.ag.return.Nmiss<-fit.ag.return.Ntotal-fit.ag.return.Nvalid
	fit.ag.return.mean<-mean(predict.outlist$fit,na.rm=TRUE)
	fit.ag.return.sd<-sqrt(stats::var(predict.outlist$fit,na.rm=TRUE))	
	fit.ag.return.quantiles<-stats::quantile(predict.outlist$fit,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)

	se.fit.ag.return.Ntotal<-length(predict.outlist$se.fit)  
	se.fit.ag.return.Nvalid<-sum(!is.na(predict.outlist$se.fit))  
	se.fit.ag.return.Nmiss<-se.fit.ag.return.Ntotal-se.fit.ag.return.Nvalid
	se.fit.ag.return.mean<-mean(predict.outlist$se.fit,na.rm=TRUE)
	se.fit.ag.return.sd<-sqrt(stats::var(predict.outlist$se.fit,na.rm=TRUE))
	se.fit.ag.return.quantiles<-stats::quantile(predict.outlist$se.fit,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)
  
	safe.list<-list(glm.object=glmname.transmit,newdfname=newdataname.transmit,output.type=output.type,
				  dispersion=dispersion,
				  fit.Ntotal=fit.ag.return.Ntotal,fit.Nvalid=fit.ag.return.Nvalid,fit.Nmiss=fit.ag.return.Nmiss,
				  fit.mean=fit.ag.return.mean,fit.sd=fit.ag.return.sd,fit.quantiles=fit.ag.return.quantiles,
				  se.fit.Ntotal=se.fit.ag.return.Ntotal,se.fit.Nvalid=se.fit.ag.return.Nvalid,se.fit.Nmiss=se.fit.ag.return.Nmiss,
				  se.fit.mean=se.fit.ag.return.mean,se.fit.sd=se.fit.ag.return.sd,se.fit.quantiles=se.fit.ag.return.quantiles,
				  residual.scale=predict.outlist$residual.scale)
	return(list(safe.list=safe.list))
  }else{
        term.names<-colnames(outlist$fit)

	#fit
	numterms<-(dim(predict.outlist$fit)[2])
	
	fit.Ntotal<-matrix(ncol=numterms,nrow=1)
	fit.Nvalid<-matrix(ncol=numterms,nrow=1)
	fit.Nmiss<-matrix(ncol=numterms,nrow=1)
	fit.mean<-matrix(ncol=numterms,nrow=1)
	fit.sd<-matrix(ncol=numterms,nrow=1)
    fit.quantiles<-matrix(ncol=numterms,nrow=7)

	for(j in 1:numterms)
	{
	fit.Ntotal[j]<-length(predict.outlist$fit[,j])  
	fit.Nvalid[j]<-sum(!is.na(predict.outlist$fit[,j]))   
	fit.Nmiss[j]<-fit.Ntotal[j]-fit.Nvalid[j]  
	fit.mean[j]<-mean(predict.outlist$fit[,j],na.rm=TRUE)
	fit.sd[j]<-sqrt(stats::var(predict.outlist$fit[,j],na.rm=TRUE))
    fit.quantiles[,j]<-stats::quantile(predict.outlist$fit[,j],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)
	}
	
	fit.matrix<-rbind(fit.Ntotal,fit.Nvalid,fit.Nmiss,fit.mean,fit.sd,fit.quantiles)
	colnames(fit.matrix)<-term.names
	rownames(fit.matrix)<-list("Ntotal","Nvalid","Nmiss","mean","stdev",
	                            "quantile_0.05","quantile_0.1","quantile_0.25","quantile_0.5",
								"quantile_0.75","quantile_0.9","quantile_0.95")
	

	#se.fit
	numterms<-(dim(predict.outlist$se.fit)[2])
	
	se.fit.Ntotal<-matrix(ncol=numterms,nrow=1)
	se.fit.Nvalid<-matrix(ncol=numterms,nrow=1)
	se.fit.Nmiss<-matrix(ncol=numterms,nrow=1)
	se.fit.mean<-matrix(ncol=numterms,nrow=1)
	se.fit.sd<-matrix(ncol=numterms,nrow=1)
    se.fit.quantiles<-matrix(ncol=numterms,nrow=7)

	for(j in 1:numterms)
	{
	se.fit.Ntotal[j]<-length(predict.outlist$se.fit[,j])  
	se.fit.Nvalid[j]<-sum(!is.na(predict.outlist$se.fit[,j]))   
	se.fit.Nmiss[j]<-se.fit.Ntotal[j]-se.fit.Nvalid[j]  
	se.fit.mean[j]<-mean(predict.outlist$se.fit[,j],na.rm=TRUE)
	se.fit.sd[j]<-sqrt(stats::var(predict.outlist$se.fit[,j],na.rm=TRUE))
	se.fit.quantiles[,j]<-stats::quantile(predict.outlist$se.fit[,j],c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)
	}
	se.fit.matrix<-rbind(se.fit.Ntotal,se.fit.Nvalid,se.fit.Nmiss,se.fit.mean,se.fit.sd,se.fit.quantiles)
	colnames(se.fit.matrix)<-term.names
	rownames(se.fit.matrix)<-list("Ntotal","Nvalid","Nmiss","mean","stdev",
	                            "quantile_0.05","quantile_0.1","quantile_0.25","quantile_0.5",
								"quantile_0.75","quantile_0.9","quantile_0.95")

	
	#residual.scale
	residual.scale<- (predict.outlist$residual.scale)
	
	}


	

	safe.list<-list(glm.object=glmname.transmit,newdfname=newdataname.transmit,output.type=output.type,
				  dispersion=dispersion,fit.matrix=fit.matrix,se.fit.matrix=se.fit.matrix,residual.scale=residual.scale)

	return(list(safe.list=safe.list))

	}

  }
 
#AGGREGATE FUNCTION
# glmPredictDS.ag



