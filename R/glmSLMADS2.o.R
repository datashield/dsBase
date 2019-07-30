#' 
#' @title glmSLMADS2.o
#' @description This is the second serverside function called by ds.glmSLMA.o.
#' @details It is an
#' aggregation function that fits the generalized linear model (glm) specified
#' in the call to ds.glmSLMA.o. The model is first constructed
#' and disclosure checked by glmDS1.o (the same serverside function called first
#' by ds.glm.o as well as by ds.glmSLMA.o). It is then fitted to convergence
#' on each study separately using glmSLMADS2.o to return parameter estimates
#' and standard errors to the client. These can then be pooled using random
#' effects meta-analysis (eg under metafor). This mode of model fitting may
#' reasonably be called study level meta-analysis (SLMA) although the analysis
#' is based on estimates and standard errors derived from direct analysis of
#' the individual level data in each study rather than from published study
#' summaries. It may also be contrasted to the approach adopted by ds.glm.o
#' via glmDS2.o which fits the glm iteratively on all studies simultaneously
#' updating the parameter estimates across all studies simultaneously as the
#' model converges. This latter approach may reasonably be called IPD
#' (individual patient data meta-analysis) and it is mathematically equivalent
#' to placing all data from all studies in one data warehouse and analysing
#' them using the standard glm() function in native R (with appropriate
#' study-specific centre effects). Both of these approaches have strengths
#' and weaknesses and this is why DataSHIELD supports both methods.  
#' For more details please see the extensive headers for ds.glm.o and
#' ds.glmSLMA.o.
#' @param formula a glm() formula consistent with R syntax eg U~x+y+Z to regress
#' variables U on x,y and Z
#' @param family a glm() family consistent with R syntax eg "gaussian", "poisson",
#' "binomial"
#' @param offset an optional variable providing a regression offset
#' @param weights an optional variable providing regression weights
#' @param dataName an optional character string specifying a data.frame object holding
#' the data to be analysed under the specified model.
#' @return model components:- glmDSDLMA2.o returns key components of model fit
#' from each study including parameter estimates and standard errors which
#' are then processed and reported by ds.glmSLMA.o potentially including
#' random effects meta-analysis using the metafor package if requested
#' in the call to ds.glmSLMA.o
#' @author Burton PR
#' @export

glmSLMADS2.o <- function(formula, family, offset, weights, dataName){
#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
#thr<-.AGGREGATE$listDisclosureSettingsDS.o()               #
#thr <- dsBetaTest::listDisclosureSettingsDS.o()            #
thr <- listDisclosureSettingsDS.o()                         #
nfilter.tab <- as.numeric(thr$nfilter.tab)                  #
nfilter.glm <- as.numeric(thr$nfilter.glm)                  #
#nfilter.subset<-as.numeric(thr$nfilter.subset)             #
#nfilter.string<-as.numeric(thr$nfilter.string)             #
#############################################################




errorMessage2<-"No errors"
# Get the value of the 'data' parameter provided as character on the client side
# Same is done for offset and weights lower down function

  if(!is.null(dataName)){
    dataDF <- eval(parse(text=dataName))
  }else{
    dataDF<-NULL
  }

 
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
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)


#Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
  model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
 
  varnames <- c()
  for(i in 1:length(model.variables)){
    elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
    if(length(elt) > 1){
      assign(elt[length(elt)], eval(parse(text=model.variables[i])))
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
        test.string<-paste0(dataName,"$","1")
        if(varnames[v]==test.string)varnames[v]<-"1"
      }
      cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  }else{
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  }
 
  #Identify and use variable names to count missings

  all.data <- eval(parse(text=cbindraw.text))

  Ntotal <- dim(all.data)[1]

  nomiss.any <- stats::complete.cases(all.data)
  nomiss.any.data <- all.data[nomiss.any,]
  N.nomiss.any <- dim(nomiss.any.data)[1]

  Nvalid <- N.nomiss.any
  Nmissing <- Ntotal-Nvalid

  formula2use <- stats::as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object

  ################################################################## 
  #sort out offset and weights
  varname.offset <- paste0(offset)

  if(!(is.null(offset)))
  {
    cbindtext.offset <- paste0("cbind(", offset,")")
    offset <- eval(parse(text=cbindtext.offset))
  }

  varname.weights<-paste0(weights)

  if(!(is.null(weights)))
  {
    cbindtext.weights <- paste0("cbind(", weights,")")
    weights <- eval(parse(text=cbindtext.weights))
  }

  ##################################################################

  mg <- stats::glm(formula2use, family=family, offset=offset, weights=weights, data=dataDF)

  outlist<-list(rank=mg$rank, aic=mg$aic,
                iter=mg$iter, converged=mg$converged,
                boundary=mg$boundary, na.action=options("na.action"), call=summary(mg)$call, terms=summary(mg)$terms,
                contrasts=summary(mg)$contrasts, aliased=summary(mg)$aliased, dispersion=summary(mg)$dispersion,
                data=dataName, df=summary(mg)$df, Ntotal=Ntotal, Nvalid=Nvalid, Nmissing=Nmissing,
                cov.unscaled=summary(mg)$cov.unscaled, cov.scaled=summary(mg)$cov.scaled,
                offset=varname.offset, weights=varname.weights,VarCovMatrix=NA,CorrMatrix=NA,
                deviance.null=mg$null.deviance, df.null=mg$df.null, deviance.resid=mg$deviance, df.resid=mg$df.residual,
                formula=mg$formula, family=mg$family,coefficients=summary(mg)$coefficients)

  return(outlist)

}
# AGGREGATE FUNCTION
# glmSLMADS2.o
