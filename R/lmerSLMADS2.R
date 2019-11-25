#' 
#' @title lmeSLMADS2
#' @description This is the serverside function called by ds.lmerSLMA.
#' @details It is an
#' aggregation function that fits the linear mixed effect model (lme) specified
#' in the call to ds.lmerSLMA. The model is first converted into a glm format
#' and disclosure checked using the standard glm process. It is then fitted to convergence
#' on each study separately using lmerSLMADS2 to return parameter estimates
#' and standard errors to the client. These can then be pooled using random
#' effects meta-analysis (eg under metafor). This mode of model fitting may
#' reasonably be called study level meta-analysis (SLMA) although the analysis
#' is based on estimates and standard errors derived from direct analysis of
#' the individual level data in each study rather than from published study
#' summaries.  
#' For more details please see the extensive headers for
#' ds.lmerSLMA.
#' @param formula a lmer() formula consistent with lme4 syntax eg U~x+y+(1|Z) to regress
#' variables U on x and y with a random effect for Z
#' @param offset an optional variable providing a regression offset
#' @param weights an optional variable providing regression weights
#' @param dataName an optional character string specifying a data.frame object holding
#' the data to be analysed under the specified model.
#' @param REML a boolean indicating whether the model should be fitted using REML
#' @param control_opt an optional variable (string) for specifying the optimiser. For glmes one or
#' two optimisers can be specified
#' @param control_tol an optional variable (numeric) to specify the value of check.conv.grad
#' @param verbose an optional variable (integer) to specify fitting information to the client side
#' @return model components:- lmerDSDLMA2 returns key components of model fit
#' from each study including parameter estimates and standard errors which
#' are then processed and reported by ds.lmerSLMA potentially including
#' random effects meta-analysis using the metafor package if requested
#' in the call to ds.lmerSLMA
#' @export
lmerSLMADS2 <- function(formula, offset, weights, dataName, REML = TRUE, control_opt = NULL, control_tol = NULL, verbose = 0){
  
  errorMessage <- "No errors"
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
   thr <- dsBase::listDisclosureSettingsDS()
   nfilter.tab <- as.numeric(thr$nfilter.tab)
   nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
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
   formula <- as.formula(formula)
  
   
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
  
  nomiss.any <- complete.cases(all.data)
  nomiss.any.data <- all.data[nomiss.any,]
  N.nomiss.any <- dim(nomiss.any.data)[1]
  
  Nvalid <- N.nomiss.any
  Nmissing <- Ntotal-Nvalid
  
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula))), env = parent.frame()) # here we need the formula as a 'call' object

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
  
  formulatext.glm = originalFormula
  
  # Convert formula string into formula string that will work for GLM
  formulatext.glm <- gsub(" ", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("(", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("(1", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("(0", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub(")", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("|", "+", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub(":", "+", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("/", "+", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("++", "+", formulatext.glm, fixed=TRUE)
  
  formula2use.glm <- as.formula(paste0(Reduce(paste, deparse(formulatext.glm ))), env = parent.frame()) # here we need the formula as a 'call' object
  
  # mod.glm.ds <- stats::glm(formula2use.glm, family="gaussian", x=TRUE, control=stats::glm.control(maxit=1), contrasts=NULL, data=dataDF)
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
    # set up control object
    
    control.obj = lme4::lmerControl()
    
    if (!is.null(control_opt)){
      control.obj$optimizer = control_opt
    }
    
    if (!is.null(control_tol)){
      control.obj$checkConv$check.conv.grad = lme4::.makeCC("warning",control_tol)
    }
    
    #mg <- lme4::lmer(formula2use, offset=offset, weights=weights, data=dataDF, REML = REML, verbose = verbose, control = control.obj)
    iterations <- capture.output(try(mg <- lme4::lmer(formula2use, offset=offset, weights=weights, data=dataDF, REML = REML, verbose = verbose, control = control.obj)))
    
    summary_mg = summary(mg)
    summary_mg$residuals <- NULL
    summary_mg$errorMessage = errorMessage
    summary_mg$disclosure.risk = disclosure.risk
    summary_mg$iterations = iterations
    outlist = summary_mg
  }
  else{
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
# lmerSLMADS2
