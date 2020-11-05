#' @title Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)
#' @description Fits a generalized linear model (GLM) on data from single or multiple sources
#' with pooled co-analysis across studies being based on SLMA (Study Level Meta Analysis).
#' @details glmSLMADS.assign is an aggregate function called by clientside function ds.glmSLMA.
#' ds.glmSLMA also calls another aggregate function glmSLMADS2
#' and an assign function glmSLMADS.assign
#' For more detailed information see help for ds.glmSLMA.
#' @param formula a glm formula, specified in call to ds.glmSLMA
#' @param family a glm family, specified in call to ds.glmSLMA
#' @param offset a character string specifying a variable to be used as an offset.
#' Specified in call to ds.glmSLMA.
#' @param weights a character string specifying a variable to be used as regression weights.
#' Specified in call to ds.glmSLMA. Specified in call to ds.glmSLMA.
#' @param newobj a character string specifying the name of the glm object
#' written to the serverside by glmSLMADS.assign. This is either the name
#' specified by the newobj argument in ds.glmSLMA or if newobj was unspecified
#' or NULL it is called new.glm.obj.
#' @param dataName a character string specifying the name of a data.frame
#' holding the data for the model. Specified in call to ds.glmSLMA.
#' @return All quantitative, Boolean, and character objects required to
#' enable the SLMA pooling of the separate glm models fitted to each study -
#' in particular including the study-specific regression coefficients and their corresponding
#' standard errors.
#' @author Paul Burton for DataSHIELD Development Team (14/7/20)
#' @export
glmSLMADS2 <- function(formula, family, offset, weights, newobj, dataName){

#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
thr <- listDisclosureSettingsDS()                           #
nfilter.tab <- as.numeric(thr$nfilter.tab)                  #
nfilter.glm <- as.numeric(thr$nfilter.glm)                  #
#nfilter.subset<-as.numeric(thr$nfilter.subset)             #
#nfilter.string<-as.numeric(thr$nfilter.string)             #
#############################################################

########################################
############
#Convert transmitable text for special link variance combinations back to full representation
if(family=="quasigamma.link_log")
{family<-"quasi(link=log,variance=mu^2)"}

if(family=="Gamma.link_log")
{family<-"Gamma(link=log)"}
#############

final.family.object<-eval(parse(text=family))
#########################################

errorMessage2<-"No errors"
# Get the value of the 'data' parameter provided as character on the client side
# Same is done for offset and weights lower down function

  if(!is.null(dataName)){
    dataDF <- eval(parse(text=dataName), envir = parent.frame())
  }else{
	dataDF<-NULL
	}

 
	#formula2use <- stats::as.formula(paste0(Reduce(paste, deparse(originalFormula))), env = parent.frame()) # here we need the formula as a 'call' object
  formula2use <- formula
  
	################################################################## 
	#sort out offset and weights
	if(is.null(offset))
	{
	varname.offset<-NULL
	#offset.to.use <- NULL
	cbindtext.offset <- paste0("offset.to.use <- NULL")
	eval(parse(text=cbindtext.offset), envir = parent.frame())
	}else{
	varname.offset <- paste0(offset)
	}

	if(!(is.null(offset)))
		{
		cbindtext.offset <- paste0("offset.to.use <- cbind(", offset,")")
		eval(parse(text=cbindtext.offset), envir = parent.frame())
		}

	if(is.null(weights))
	{
	varname.weights<-NULL
	cbindtext.weights <- paste0("weights.to.use <- NULL")
	eval(parse(text=cbindtext.weights), envir = parent.frame())
	#weights.to.use <- NULL
	}else{
	varname.weights <- paste0(weights)
	}


	if(!(is.null(weights)))
		{
		cbindtext.weights <- paste0("weights.to.use <- cbind(", weights,")")
		eval(parse(text=cbindtext.weights), envir = parent.frame())
		#cbindtext.weights <- paste0("cbind(", weights,")")
		#weights.to.use <- eval(parse(text=cbindtext.weights), envir = parent.frame())
		}

#This function call now undertaken in glmDS.assign function and is here replaced by
#bringing back in the mg output saved from that previous call
#	mg <- stats::glm(formula2use, family=final.family.object, x=TRUE, offset=offset.to.use, weights=weights.to.use, data=dataDF)

activate.text<- paste0("mg<-",newobj)
eval(parse(text=activate.text))
	
y.vect<-mg$y
X.mat<-mg$x
pw.vect<-mg$prior.weights
offset.vect<-mg$offset
 


##############################
#TEST FOR OVERSATURATED MODEL#
##############################
 dimX<-dim((X.mat))

 glm.saturation.invalid<-0
 num.p<-as.numeric(dimX[2])
 num.N<-as.numeric(dimX[1])
   
if(num.p>(nfilter.glm*num.N)){
glm.saturation.invalid<-1
errorMessage.gos<-paste0("ERROR: Model is oversaturated (too many model parameters relative to sample size)",
                         "leading to a possible risk of disclosure - please simplify model. With ",
						 num.p," parameters and nfilter.glm = ",round(nfilter.glm,4)," you need ",
						 round((num.p/nfilter.glm),0)," observations")
}



#########################
#CHECK Y VECTOR VALIDITY#
#########################

	y.invalid<-0

#Count number of unique non-missing values - disclosure risk only arises with two levels
    unique.values.noNA.y<-unique(y.vect[stats::complete.cases(y.vect)])

#If two levels, check whether either level 0 < n < nfilter.tab
	if(length(unique.values.noNA.y)==2){
		tabvar<-table(y.vect)[table(y.vect)>=1]   #tabvar counts n in all categories with at least one observation
		min.category<-min(tabvar)
		if(min.category<nfilter.tab){
		   y.invalid<-1
		   errorMessage.y<-"ERROR: y (response) vector is binary with one category less than filter threshold for table cell size"
		   }
		}


#Check x matrix validity 
#Check no dichotomous x vectors with between 1 and filter.threshold 
#observations at either level 

	Xpar.invalid<-rep(0,num.p)
	x.invalid<-0 #Any x parameter invalud

        for(pj in 1:num.p){
	unique.values.noNA<-unique((X.mat[,pj])[stats::complete.cases(X.mat[,pj])]) 

	if(length(unique.values.noNA)==2){
		tabvar<-table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar counts n in all categories with at least one observation
		min.category<-min(tabvar)
		if(min.category<nfilter.tab){
		    Xpar.invalid[pj]<-1
			x.invalid<-1
		    errorMessage.x<-"ERROR: at least one column in X matrix is binary with one category less than filter threshold for table cell size"
            }
	   }
	}




#Check w vector validity
	w.invalid<-0

if(!is.null(pw.vect))
{
	w.vect<-pw.vect

	unique.values.noNA.w<-unique(w.vect[stats::complete.cases(w.vect)])

	if(length(unique.values.noNA.w)==2){
		tabvar<-table(w.vect)[table(w.vect)>=1]   #tabvar counts n in all categories with at least one observation
		min.category<-min(tabvar)
		if(min.category<nfilter.tab){
        w.invalid<-1
		errorMessage.w<-"ERROR: weights vector is binary with one category less than filter threshold for table cell size"
            }
	}
}

#Check o vector validity
	o.invalid<-0

if(!is.null(offset.vect))
{	
	#Keep vector name consistent
	o.vect<-offset.vect

	unique.values.noNA.o<-unique(o.vect[stats::complete.cases(o.vect)])

	if(length(unique.values.noNA.o)==2){
		tabvar<-table(o.vect)[table(o.vect)>=1]   #tabvar counts n in all categories with at least one observation
		min.category<-min(tabvar)
		if(min.category<nfilter.tab){
        o.invalid<-1
		errorMessage.o<-"ERROR: offset vector is binary with one category less than filter threshold for table cell size"
            }
	}
}
	
###############################################
#FIT MODEL AFTER CONFIRMING NO DISCLOSURE RISK#
###############################################




disclosure.risk<-0

if(y.invalid>0||w.invalid>0||o.invalid>0||sum(Xpar.invalid)>0||glm.saturation.invalid>0){
disclosure.risk<-1
	}

if(disclosure.risk==0)
{
#	mg <- stats::glm(formula2use, family=final.family.object, offset=offset.to.use, weights=weights.to.use, data=dataDF)
  
	Nvalid <- length(mg$residuals)
	Nmissing <- length(mg$na.action)
	Ntotal <- Nvalid+Nmissing
	
	outlist<-list(rank=mg$rank, aic=mg$aic, 
             iter=mg$iter, converged=mg$converged,
			 boundary=mg$boundary, na.action=options("na.action"), call=summary(mg)$call, terms=summary(mg)$terms,
			 contrasts=summary(mg)$contrasts, aliased=summary(mg)$aliased, dispersion=summary(mg)$dispersion,
			 data=dataName, df=summary(mg)$df, Ntotal=Ntotal, Nvalid=Nvalid, Nmissing=Nmissing,
			 cov.unscaled=summary(mg)$cov.unscaled, cov.scaled=summary(mg)$cov.scaled,
			 offset=varname.offset, weights=varname.weights,VarCovMatrix=NA,CorrMatrix=NA,
			 deviance.null=mg$null.deviance, df.null=mg$df.null, deviance.resid=mg$deviance, df.resid=mg$df.residual,
			 formula=mg$formula, family=mg$family,coefficients=summary(mg)$coefficients)
}else{
		errorMessage.d1<-"ERROR: Model failed in this source because of an enhanced risk of disclosure"
		errorMessage.d2<-"The following message(s) identify the cause of this enhanced risk"
		
		outlist.1<-list(errorMessage.1=errorMessage.d1)
		outlist.2<-list(errorMessage.2=errorMessage.d2)

		outlist.gos<-NULL
		if(glm.saturation.invalid==1){
		outlist.gos<-list(errorMessage.gos=errorMessage.gos)		
		}

		outlist.y<-NULL
		if(y.invalid==1){
		outlist.y<-list(errorMessage.y=errorMessage.y)		
		}

		outlist.x<-NULL
		if(x.invalid==1){
		outlist.x<-list(errorMessage.x=errorMessage.x)		
		}

		outlist.w<-NULL
		if(w.invalid==1){
		outlist.w<-list(errorMessage.w=errorMessage.w)		
		}

		outlist.o<-NULL
		if(o.invalid==1){
		outlist.o<-list(errorMessage.o=errorMessage.o)		
		}

    outlist<-list(outlist.1,outlist.2,outlist.gos,outlist.y,outlist.x,outlist.w,outlist.o)
}
	#tidy up in parent.frame()
	eval(quote(rm(offset.to.use)), envir = parent.frame())
	eval(quote(rm(weights.to.use)), envir = parent.frame())
	return(outlist)

}
# AGGREGATE FUNCTION
# glmSLMADS2
