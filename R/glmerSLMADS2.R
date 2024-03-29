#' @title Fitting generalized linear mixed effect models - serverside function
#' @description glmerSLMADS2 fits a generalized linear mixed effects model
#' (glme) - e.g. a logistic or Poisson regression model including both fixed and random effects
#' - on data from one or multiple sources with pooling via SLMA (study level meta-analysis)
#' @details  glmerSLMADS2 is a serverside function called by ds.glmerSLMA on the clientside.
#' The analytic work engine is the glmer function in R which sits in the lme4 package.
#' ds.glmerSLMA fits a generalized linear mixed effects model (glme) - e.g. a logistic or
#' Poisson regression model including both fixed and random effects - on data
#' from a single or multiple sources. When there are multiple data sources,
#' the glme is fitted to convergence in each data source independently and the
#' estimates and standard errors returned to the client thereby enabling cross-study pooling
#' using study level meta-analysis (SLMA). By default the SLMA is undertaken
#' using the metafor package, but as the SLMA occurs on the clientside which, as far
#' as the user is concerned is just a standard R environment, the user can choose to use
#' any approach to meta-analysis they choose. Additional information about fitting 
#' glmes using the glmer engine can be obtained using R help for glmer and the lme4 package
#' @param formula see help for ds.glmerSLMA
#' @param offset see help for ds.glmerSLMA
#' @param weights see help for ds.glmerSLMA
#' @param dataName see help for ds.glmerSLMA
#' @param family see help for ds.glmerSLMA
#' @param control_type see help for ds.glmerSLMA
#' @param control_value.transmit see help for argument <control_value> for
#' function ds.glmerSLMA 
#' @param nAGQ integer scalar, defaulting to 1L. IN PRACTICE, IT MAY BE NECESSARY
#' TO SET nAGQ TO 0L when the model appears to converge perfectly well (e.g. verbose=2
#' demonstrates good initial convergence of both the log-likelihood and
#' regression coefficients) but formal convergence does not get declared - 
#' so no output is produced - despite running the model for many iterations.
#' The nAGQ argument is set by the nAGQ argument for ds.glmerSLMA and further
#' details can be found in help(ds.glmerSLMA) and in the native R help for glmer() 
#' @param verbose see help for ds.glmerSLMA
#' @param theta see help for argument <start_theta> for
#' function ds.glmerSLMA
#' @param fixef see help for argument <start_fixef> for
#' function ds.glmerSLMA
#' @return all key model components see help for ds.glmerSLMA
#' @author Tom Bishop, with some additions by Paul Burton
#' @export
glmerSLMADS2 <- function(formula, offset, weights, dataName, family,
                control_type=NULL, control_value.transmit=NULL, nAGQ=1L, verbose = 0, theta = NULL, fixef = NULL){
 
  errorMessage <- "No errors"

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  nfilter.glm <- as.numeric(thr$nfilter.glm)
  #############################################################
  
  # Get the value of the 'data' parameter provided as character on the client side
  # Same is done for offset and weights lower down function
  
  if(!is.null(dataName)){
    dataDF <- eval(parse(text=dataName), envir = parent.frame())
  }else{
    dataDF <- NULL
  }
  
  # Put pipes back into formula
  #formula = as.formula(paste(formula,collapse="|"))
   formula <- Reduce(paste, deparse(formula))
   formula <- gsub("xxx", "|", formula, fixed = TRUE)
   formula <- gsub("yyy", "(", formula, fixed = TRUE)
   formula <- gsub("zzz", ")", formula, fixed = TRUE)
   formula <- gsub("ppp", "/", formula, fixed = TRUE)
   formula <- gsub("qqq", ":", formula, fixed = TRUE)
   formula <- stats::as.formula(formula)
  
   
  # Rewrite formula extracting variables nested in strutures like data frame or list
  # (e.g. D$A~D$B will be re-written A~B)
  # Note final product is a list of the variables in the model (yvector and covariates)
  # it is NOT a list of model terms - these are derived later
  
  # Convert formula into an editable character string
  formulatext <- Reduce(paste, deparse(formula))
  
  # First save original model formala
  originalFormula <- formulatext
  
  # Convert formula string into separate variable names split by |
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("(", "", formulatext, fixed=TRUE)
  formulatext <- gsub("(1", "", formulatext, fixed=TRUE)
  formulatext <- gsub("(0", "", formulatext, fixed=TRUE)
  formulatext <- gsub(")", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("/", "|", formulatext, fixed=TRUE)
  formulatext <- gsub(":", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)
  
  
  # Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
  model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
  
  varnames <- c()
  for(i in 1:length(model.variables)){
    elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
    if(length(elt) > 1){
      assign(elt[length(elt)], eval(parse(text=model.variables[i]), envir = parent.frame()), envir = parent.frame())
      originalFormula.modified <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
      varnames <- append(varnames, elt[length(elt)])
    }else{
      varnames <- append(varnames, elt)
    }
  }
  varnames <- unique(varnames)
  
  if(!is.null(dataName)){
    for(v in 1:length(varnames)){
      varnames[v]<-paste0(dataName,"$",varnames[v])
      test.string.0 <- paste0(dataName,"$","0")
      test.string.1 <- paste0(dataName,"$","1")
      if(varnames[v]==test.string.0) varnames[v] <- "0"
      if(varnames[v]==test.string.1) varnames[v] <- "1"
    }
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")		
  }else{
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  }
  
  # Identify and use variable names to count missings
  all.data <- eval(parse(text=cbindraw.text), envir = parent.frame())
  
  Ntotal <- dim(all.data)[1]
  
  nomiss.any <- stats::complete.cases(all.data)
  nomiss.any.data <- all.data[nomiss.any,]
  N.nomiss.any <- dim(nomiss.any.data)[1]
  
  Nvalid <- N.nomiss.any
  Nmissing <- Ntotal-Nvalid
  
  formula2use <- stats::as.formula(paste0(Reduce(paste, deparse(formula))), env = parent.frame()) # here we need the formula as a 'call' object

  ################################################################## 
  #sort out offset and weights
  varname.offset <- paste0(offset)
  
  if(!(is.null(offset))){
    cbindtext.offset <- paste0("cbind(", offset,")")
    offset <- eval(parse(text=cbindtext.offset), envir = parent.frame())
  }
  else{
    assign(x = 'offset', value = NULL, envir = parent.frame())
  }
  
  varname.weights<-paste0(weights)
  
  if(!(is.null(weights))){
    cbindtext.weights <- paste0("cbind(", weights,")")
    weights <- eval(parse(text=cbindtext.weights), envir = parent.frame())
  }
  else{
    assign(x = 'weights', value = NULL, envir = parent.frame())
  }
  
  #### BEFORE going further we use the glm1 checks
  
  # This command creates a formula object without the grouping factors for running the standard glm checks
  
  formula2use.glm <- lme4::nobars(formula2use)
  
  mod.glm.ds <- stats::glm(formula2use.glm, family="gaussian", x=TRUE, offset=offset, weights=weights, data=dataDF)

  y.vect<-mod.glm.ds$y
  X.mat<-mod.glm.ds$x
  pw.vect<-mod.glm.ds$prior.weights
  offset.vect<-mod.glm.ds$offset
  
  
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
  x.invalid<-0 #Any x parameter invalid
  
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

#########################
# set up control object #
#########################
   
#Temporary code to ensure control.obj is non-null and
#defaults to standard default glmerControl values 
    control.obj<-lme4::glmerControl()


#Construct control object
control.text<-"lme4::glmerControl("


#Sort control_value 
if(!is.null(control_type)){
	
	if(!is.null(control_value.transmit))
		{
		if(!is.numeric(control_value.transmit))
		  {
		  control_value<-as.numeric(control_value.transmit)
		  }

		if(is.numeric(control_value.transmit))
		  {
		  control_value<-control_value.transmit
		  }
		}

}

if(is.null(control_type)){
	  control_value<-NULL
	 }	



#Sort control_type
if(!is.null(control_type)){

#Code for check.conv.grad
if(control_type=="check.conv.grad")
		{
			default.value<-1.0e-3 #default for glmerControl
			
			if(is.null(control_value)){
			control_value<-default.value
			}
      full.control.text<-paste0(control.text,"check.conv.grad = lme4::.makeCC('warning', tol=",control_value,", relTol=NULL))")
			control.obj<-eval(parse(text=full.control.text))
		}	
		
    }

####################################################################################
#THIS NEXT BLOCK OF CODE REMOVED BECAUSE AT PRESENT ANY NON-DEFAULT OPTIMIZER WILL 
#ALMOST CERTAINLY REQUIRE CODE MODIFICATION AND THE DEFAULT CONSISTS OF THE
#TWO DIFFERENT OPTIMIZERS FOR STAGE 1 AND STAGE 2 WHICH IS SO SPECIFIC
#IT IS BETTER TO AVOID ALLOWING THE OPTIMIZER TO BE SELECTED IN THE CLIENTSIDE
#if(!is.null(optimizer)&&optimizer!="bobyqa"&&optimizer!="Nelder_Mead")
# 	{
#	control.obj$optimizer<-"Nelder_Mead" #default for glmerControl
#	}
#	
#if(is.null(optimizer))
# 	{
#	control.obj$optimizer<-"Nelder_Mead" #default for glmerControl
#	}
#############################################################################
    
 
 # sort out start values
    start = NULL
    if(!is.null(fixef)){
      fixef <- as.numeric(unlist(strsplit(fixef, split=",")))
    }
    if(!is.null(theta)){
      theta <- as.numeric(unlist(strsplit(theta, split=",")))
      if(is.null(fixef)){
        start = theta
      }else{
        start = list(theta=theta, fixef=fixef)
      }
    }
    
    
	  #iterations <- utils::capture.output(try(mg <- lme4::glmer(formula2use, offset=offset, weights=weights, data=dataDF,
	 #                                      family = family, nAGQ=nAGQ,verbose = verbose, control=control.obj, start = start)))
	  
	  iterations = utils::capture.output(mg <- try(lme4::glmer(formula2use, offset=offset, weights=weights, data=dataDF,
	                                                           family = family, nAGQ=nAGQ,verbose = verbose, control=control.obj, start = start)))
	  
	  if(inherits(mg, "try-error")) {          # error happened
	    summary_mg = list()
	    summary_mg$errorMessage = conditionMessage(attr(mg, "condition"))  # the error message
	    summary_mg$disclosure.risk = 1
	  } else
	  {
	    summary_mg = summary(mg)
	    summary_mg$residuals <- NULL
	    summary_mg$errorMessage <- errorMessage
	    summary_mg$disclosure.risk <- disclosure.risk
	    summary_mg$iterations <- iterations
	    summary_mg$control.info <- control.obj
	    summary_mg$nAGQ.info <- nAGQ
	  }


    outlist <- summary_mg
    
    outlist$Ntotal <- Ntotal
    outlist$Nvalid <- Nvalid
    outlist$Nmissing <- Nmissing
    
  }else{ #i.e. if disclosure.risk !=0
    
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

    outlist<-list(outlist.1,outlist.2,outlist.gos,outlist.y,outlist.x,outlist.w,outlist.o, disclosure.risk = disclosure.risk)
 
 }
  return(outlist)
  
}
# AGGREGATE FUNCTION
# glmerSLMADS2
