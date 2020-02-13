#' @title glmSLMADS2 called by ds.glmSLMA
#' @description This is the second serverside aggregate function
#' called by ds.glmSLMA
#' @details ds.glmSLMA specifies the structure of a generalized linear model (glm)
#' to be fitted separately on each study. The model is first constructed
#' and subject to preliminary disclosure checking
#' by glmSLMADS1. This aggregate function then returns this 
#' output to ds.glmSLMA which processes the information and uses it in a call to
#' glmSLMADS2. This call specifies and fits the required glm in each data source.
#' Unlike glmDS2 (called by the more commonly used generalized linear modelling
#' client-side function ds.glm) the requested model is then fitted to completion
#' on the data in each study rather than iteration by iteration on all studies
#' combined. At the end of this SLMA fitting process
#' glmSLMADS2 returns study-specific parameter estimates
#' and standard errors to the client. These can then be pooled using random
#' effects (or fixed effects) meta-analysis - eg using the metafor package.
#' This mode of model fitting may
#' reasonably be called study level meta-analysis (SLMA) although the analysis
#' is based on estimates and standard errors derived from direct analysis of
#' the individual level data in each study rather than from published study
#' summaries (as is often the case with SLMA of clinical trials etc).
#' Furthermore, unlike common approaches to study-level meta-analysis
#' adopted by large multi-study research consortia (eg in the combined analysis
#' of identical genomic markers across multiple studies), the parallel
#' analyses (in every study) under ds.glmSLMA are
#' controlled entirely from one client. This avoids the time-consuming
#' need to ask each study to run its own analyses and the consequent
#' necessity to request additional work from individual studies if
#' the modelling is to be extended to include analyses not subsumed
#' in the original analytic plan. Additional analyses of this nature
#' may, for example, include analyses based on interactions between covariates
#' identified as having significant main effects in the original analysis. 
#' From a mathematical perspective, the SLMA approach (using ds.glmSLMA)
#' differs fundamentally from the usual approach using ds.glm
#' in that the latter is mathematically equivalent
#' to placing all individual-level data from all sources in
#' one central warehouse and analysing those data as one combined dataset using the
#' conventional glm() function in R. However, although this
#' may sound to be preferable under all circumstances, the SLMA approach
#' actually offers key inferential advantages when there is marked heterogeneity
#' between sources that cannot simply be corrected with fixed effects each reflecting a study
#' or centre-effect. In particular, fixed effects cannot simply be used in this way when there
#' there is heterogeneity in the effect that is of scientific interest. 
#' For more details please see the extensive header for ds.glmSLMA
#' in DataSHIELD and help on the {glm} function in native R.
#' @param formula a glm() formula consistent with R syntax eg U~x+y+Z to regress
#' variables U on x,y and Z. Fully specified by <formula> argument in ds.glmSLMA
#' @param family a glm() family consistent with R syntax eg "gaussian", "poisson",
#' "binomial". Fully specified by <family> argument in ds.glmSLMA
#' @param offset an optional variable name (as a character string)
#' identifying an offset vector. Fully specified by <offset> argument in ds.glmSLMA
#' @param weights an optional variable name (as a character sting)
#' identifying a vector of prior regression weights. Fully specified by <weights>
#' argument in ds.glmSLMA
#' @param dataName an optional character string specifying the name of a data.frame
#' object holding the data to be analysed under the specified model.
#' Fully specified by <dataName> argument in ds.glmSLMA
#' @return All quantitative, Boolean, and character objects required to
#' enable the SLMA pooling of the separate glm models fitted to each study -
#' in particular including the study-specific regression coefficients and their corresponding
#' standard errors. Also, returns warning flags
#' and associated information enabling DataSHIELD to halt analysis and destroy
#' model output from any
#' given study if a disclosure trap is triggered and to inform the user
#' what trap has been triggered.
#' @author Burton PR
#' @export
glmSLMADS2 <- function(formula, family, offset, weights, dataName){

#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
thr <- listDisclosureSettingsDS()                           #
nfilter.tab <- as.numeric(thr$nfilter.tab)                  #
nfilter.glm <- as.numeric(thr$nfilter.glm)                  #
#nfilter.subset<-as.numeric(thr$nfilter.subset)             #
#nfilter.string<-as.numeric(thr$nfilter.string)             #
#############################################################

errorMessage2<-"No errors"
# Get the value of the 'data' parameter provided as character on the client side
# Same is done for offset and weights lower down function

  if(!is.null(dataName)){
    dataDF <- eval(parse(text=dataName), envir = parent.frame())
  }else{
	dataDF<-NULL
	}

 
# Rewrite formula extracting variables nested in strutures like data frame or list
# (e.g. D$A~D$B will be re-written A~B)
# Note final product is a list of the variables in the model (yvector and covariates)
# it is NOT a list of model terms - these are derived later

# # Convert formula into an editable character string
#   formulatext <- Reduce(paste, deparse(formula))
# 
# # First save original model formala
#   originalFormula <- formulatext
# 
# # Convert formula string into separate variable names split by |
#   formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
#   formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
#   formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
#   formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
#   formulatext <- gsub("||", "|", formulatext, fixed=TRUE)

# #Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
# 	model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
# 	
# 	varnames <- c()
# 	for(i in 1:length(model.variables)){
#     elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
#     varnames <- append(varnames, elt[length(elt)])
# 	}
# 	
# 	varnames <- unique(varnames)
# 
#   if(!is.null(dataName)){
#       for(v in 1:length(varnames)){
# 	varnames[v] <- paste0(dataName,"$",varnames[v])
# 	test.string.0 <- paste0(dataName,"$","0")
# 	test.string.1 <- paste0(dataName,"$","1")
# 	if(varnames[v]==test.string.0) varnames[v] <- "0"
# 	if(varnames[v]==test.string.1) varnames[v] <- "1"
#       }
# 	  	cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")	
#   }else{
#   	    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
# 		}
#  
# 	#Identify and use variable names to count missings
# 
# 	#all.data <- eval(parse(text=cbindraw.text), envir = parent.frame())
# 	all.data <- eval(parse(text=cbindraw.text))
# 	
# 	Ntotal <- dim(all.data)[1]
# 	
# 	nomiss.any <- stats::complete.cases(all.data)
# 	nomiss.any.data <- all.data[nomiss.any,]
# 	N.nomiss.any <- dim(nomiss.any.data)[1]
# 
# 	Nvalid <- N.nomiss.any
# 	Nmissing <- Ntotal-Nvalid

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

	mg <- stats::glm(formula2use, family=family, x=TRUE, offset=offset.to.use, weights=weights.to.use, data=dataDF)
	
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
	mg <- stats::glm(formula2use, family=family, offset=offset.to.use, weights=weights.to.use, data=dataDF)
  
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



