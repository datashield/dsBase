#' 
#' @title glmDS1 called by ds.glm
#' @description This is the first server-side aggregate function called by ds.glm
#' @details It is an
#' aggregation function that sets up the model structure
#' and creates the starting beta.vector that feeds, via ds.glm, into glmDS2 to enable
#' iterative fitting of the generalized linear model that has been
#' been specified. For more details please see the extensive header for ds.glm.
#' @param formula a glm() formula consistent with R syntax eg U~x+y+Z to regress
#' variables U on x,y and Z
#' @param family a glm() family consistent with R syntax eg "gaussian", "poisson",
#' "binomial"
#' @param weights an optional variable providing regression weights
#' @param offset the offset
#' @param data an
#' optional character string specifying a data.frame object holding the data to be
#' analysed under the specified model
#' 
#' @return List with values from GLM model.
#' 
#' @author Burton PR for DataSHIELD Development Team
#' @export
#'
glmDS1 <- function(formula, family, weights, offset, data){
  
  
#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS
thr <- dsBase::listDisclosureSettingsDS()
nfilter.tab <- as.numeric(thr$nfilter.tab)
nfilter.glm <- as.numeric(thr$nfilter.glm)
#nfilter.subset <- as.numeric(thr$nfilter.subset)
#nfilter.string <- as.numeric(thr$nfilter.string)
#############################################################
 

 
  errorMessage <- "No errors"

  # get the value of the 'data' and 'weights' parameters provided as character on the client side
  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data), envir = parent.frame())
  }
  
  formulatext <- Reduce(paste, deparse(formula))
  originalFormula <- formulatext
  
  # Convert formula string into separate variable names split by |
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)
  
  originalFormulaFormula <- stats::as.formula(originalFormula)
  formula2use <- stats::as.formula(paste0(Reduce(paste, deparse(originalFormulaFormula))), env = parent.frame()) # here we need the formula as a 'call' object
  mod.glm.ds <- stats::glm(formula2use, family=family, x=TRUE, control=stats::glm.control(maxit=1), contrasts=NULL, data=dataTable)
  
  #Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
  model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
  
  varnames <- c()
  for(i in 1:length(model.variables)){
    elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
    if(length(elt) > 1){
      assign(elt[length(elt)], eval(parse(text=model.variables[i]), envir = parent.frame()), envir = parent.frame())
      originalFormula <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
      varnames <- append(varnames, elt[length(elt)])
    }else{
      varnames <- append(varnames, elt)
    }
  }
  
  varnames <- unique(varnames) 
  
  X.mat <- as.matrix(mod.glm.ds$x)
  
  dimX <- dim((X.mat))
    
  y.vect <- as.vector(mod.glm.ds$y)
    
  ##############################################################
  #FIRST TYPE OF DISCLOSURE TRAP - TEST FOR OVERSATURATED MODEL#
  #TEST AGAINST nfilter.glm									  #
  ##############################################################
  
  glm.saturation.invalid <- 0
  num.p <- dimX[2]
  num.N <- dimX[1]
  
  if(num.p>nfilter.glm*num.N){
    glm.saturation.invalid <- 1
    errorMessage <- "ERROR: Model has too many parameters, there is a possible risk of disclosure - please simplify model"
    #DELETE return(errorMessage) 
  }
    
  coef.names <- names(mod.glm.ds$coefficients)
  
  if(is.null(weights)){
    w.vect <- rep(1,length(y.vect))
  }else{
    ftext <- paste0("cbind(",weights,")")
    w.vect <- eval(parse(text=ftext), envir = parent.frame())
  }
  
    if(is.null(offset)){
    offsetvar <- rep(1,length(y.vect))
  }else{
    active.text <- paste0("cbind(",offset,")")
    offsetvar <- eval(parse(text=active.text), envir = parent.frame())
  }

  ################################
  #SECOND TYPE OF DISCLOSURE TRAP#
  ################################
  
  #If y, X or w data are invalid but user has modified clientside
  #function (ds.glm) to circumvent trap, model will get to this point without
  #giving a controlled shut down with a warning about invalid data.
  #So as a safety measure, we will now use the same test that is used to
  #trigger a controlled trap in the clientside function to destroy the
  #score.vector and information.matrix in the study with the problem.
  
  #CHECK Y VECTOR VALIDITY
  y.invalid <- 0
  
  #COUNT NUMBER OF UNIQUE NON-MISSING VALUES - DISCLOSURE RISK ONLY ARISES WITH TWO LEVELS
  unique.values.noNA.y <- unique(y.vect[stats::complete.cases(y.vect)])
  
  #IF TWO LEVELS, CHECK WHETHER EITHER LEVEL 0 < n < nfilter.tab
  
  if(length(unique.values.noNA.y)==2){
    tabvar<-table(y.vect)[table(y.vect)>=1]   #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
    min.category<-min(tabvar)
    if(min.category<nfilter.tab){
      y.invalid<-1
      errorMessage<-"ERROR: y vector is binary with one category less than filter threshold for table cell size"
    }
  }
  
  #CHECK X MATRIX VALIDITY 
  #Check no dichotomous X vectors with between 1 and filter.threshold 
  #observations at either level 
  dimX<-dim((X.mat))
  
  num.Xpar<-dimX[2]
  
  Xpar.invalid<-rep(0,num.Xpar)
  
  for(pj in 1:num.Xpar){
    unique.values.noNA<-unique((X.mat[,pj])[stats::complete.cases(X.mat[,pj])]) 
    
    if(length(unique.values.noNA)==2){
      tabvar<-table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
      min.category<-min(tabvar)
      if(min.category<nfilter.tab){
        Xpar.invalid[pj]<-1
        errorMessage<-"ERROR: at least one column in X matrix is binary with one category less than filter threshold for table cell size"
      }
    }
  }
  
  
  #CHECK W VECTOR VALIDITY

  w.invalid<-0

if(!is.null(w.vect))
{  
  unique.values.noNA.w<-unique(w.vect[stats::complete.cases(w.vect)])
  
  if(length(unique.values.noNA.w)==2){
    tabvar<-table(w.vect)[table(w.vect)>=1]   #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
    min.category<-min(tabvar)
    if(min.category<nfilter.tab){
      w.invalid<-1
      errorMessage<-"ERROR: w vector is binary with one category less than filter threshold for table cell size"
    }
  }
}
    #Check o vector validity
	o.invalid<-0

if(!is.null(offsetvar))
{	
	#Keep vector name consistent
	o.vect<-offsetvar

	unique.values.noNA.o<-unique(o.vect[stats::complete.cases(o.vect)])

	if(length(unique.values.noNA.o)==2){
		tabvar<-table(o.vect)[table(o.vect)>=1]   #tabvar counts n in all categories with at least one observation
		min.category<-min(tabvar)
		if(min.category<nfilter.tab){
        o.invalid<-1
		errorMessage<-"ERROR: offset vector is binary with one category less than filter threshold for table cell size"
            }
	}
}
  
  #If y, X or w data are invalid, or the model is overparameterized, this will be detected by glmDS1
  #and passed to ds.glm resulting in a warning and a controlled shut down of the function.
  #But in case someone modifies the client side function to circumvent the trap, so the
  #error is only apparent once the main main iterations have started via glmDS2
  #the equivalent tests in glmDS2 will destroy the info.matrix and score.vector in the affected study so
  #the model fitting will simply terminate.
  
    if(!(y.invalid>0||w.invalid>0||o.invalid>0||sum(Xpar.invalid)>0||glm.saturation.invalid>0)){
	errorMessage<-"No errors"
	}else
		{
		errorMessage<-"STUDY DATA OR APPLIED MODEL INVALID FOR THIS SOURCE"
		}
		
  return(list(dimX=dimX,coef.names=coef.names,y.invalid=y.invalid,Xpar.invalid=Xpar.invalid,w.invalid=w.invalid,o.invalid=o.invalid,
              glm.saturation.invalid=glm.saturation.invalid,errorMessage=errorMessage))
  
}
#AGGREGATE FUNCTION
# glmDS1


