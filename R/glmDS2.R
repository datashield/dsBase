#' 
#' @title glmDS2 called by ds.glm
#' @description This is the second server-side aggregate function called by ds.glm.
#' @details It is an aggregate function that uses the model structure and starting
#' beta.vector constructed by glmDS1 to iteratively fit the generalized linear model
#' that has been specified. The function glmDS2 also carries out a series of disclosure
#' checks and if the arguments or data fail any of those tests,
#' model construction is blocked and an appropriate serverside error message is
#' created and returned to ds.glm on the clientside.
#' For more details please see the extensive header for ds.glm.
#' @param formula a glm() formula consistent with R syntax eg U~x+y+Z to regress
#' variables U on x, y and Z
#' @param family a glm() family consistent with R syntax eg "gaussian", "poisson",
#' "binomial"
#' @param beta.vect a numeric vector created by the clientside function specifying the
#' vector of regression coefficients at the current iteration
#' @param offset an optional variable providing a regression offset
#' @param weights an optional variable providing regression weights
#' @param dataName an optional character string specifying a data.frame object holding
#' the data to be analysed under the specified model same 
#' 
#' @author Paul Burton, for DataSHIELD Development Team
#' 
#' @return List with values from GLM model
#' @export
#'
glmDS2 <- function (formula, family, beta.vect, offset, weights, dataName) {
  
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
      assign(elt[length(elt)], eval(parse(text=model.variables[i]), envir = parent.frame()), envir = parent.frame())
      originalFormula.modified <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
      varnames <- append(varnames, elt[length(elt)])
    }else{
      varnames <- append(varnames, elt)
    }
  }
  varnames <- unique(varnames)
  
  #varnames.with.df<-varnames
  
  if(!is.null(dataName)){
    for(v in 1:length(varnames)){
      varnames[v] <- paste0(dataName,"$",varnames[v])
      test.string.0 <- paste0(dataName,"$","0")
      test.string.1 <- paste0(dataName,"$","1")
      if(varnames[v]==test.string.0) varnames[v] <- "0"
      if(varnames[v]==test.string.1) varnames[v] <- "1"
    }
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  }else{
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  }
  
  #Identify and use variable names to count missings
  #	cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  
  all.data <- eval(parse(text=cbindraw.text), envir = parent.frame())

  #WORKS TO HERE
  
  ############################
  
  Ntotal <- dim(all.data)[1]
  
  nomiss.any <- stats::complete.cases(all.data)
  nomiss.any.data <- all.data[nomiss.any,]
  N.nomiss.any <- dim(nomiss.any.data)[1]
  
  Nvalid <- N.nomiss.any
  Nmissing <- Ntotal-Nvalid
    
  #######################################
  
  # Now fit model specified in formula: by using x=TRUE this is how we generate all of the model terms
  # and the data that underlie them. This will include a vector of 1s for the intercept and
  # any dummy variables required for factors
  
  originalFormulaFormula <- stats::as.formula(originalFormula)
  formula2use <- stats::as.formula(paste0(Reduce(paste, deparse(originalFormulaFormula))), env = parent.frame()) # here we need the formula as a 'call' object
  mod.glm.ds <- stats::glm(formula2use, family=family, x=TRUE, control=stats::glm.control(maxit=1), contrasts=NULL, data=dataDF)
  
  X.mat.orig <- as.matrix(mod.glm.ds$x)
  y.vect.orig <- as.vector(mod.glm.ds$y)
  f <- mod.glm.ds$family
  
  # Remove rows of offset or weights which contain NA in any Y or X variable
  # Rows where offset or weights are missing but Y and X are non-NA, remain at this stage
  cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ")")
  dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
  # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
  row.noNA.YX <- stats::complete.cases(dtemp)
  
  # Both weights and offset
  if(!(is.null(weights))&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ",", offset,")")
    dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]] 
    weightsvar.orig <- cmplt[, (dim(cmplt)[2]-1)]    
  }
  
  # Offset no weights 
  if(is.null(weights)&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", offset, ")")
    dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]]
  }
  
  # Weights no offset
  if(!(is.null(weights))&&(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ")")
    dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    weightsvar.orig <- cmplt[, dim(cmplt)[2]]    
  } 
  
  # Now work with y vector and X matrix from actual model (with all terms explicit)
  
  # Strip rows of y, X matrix, offset and weights if missing values in offset or weights
  # If an offset is not specified then NAs in it are meaningless and so have no impact
  # If weights are not specified then NAs in it are meaningless and so have no impact
  
  # Both weights and offset
  if(!(is.null(weights))&&!(is.null(offset))){
    YXWO.orig <- cbind(y.vect.orig,X.mat.orig,weightsvar.orig,offsetvar.orig)
    YXWO.complete <- YXWO.orig[stats::complete.cases(YXWO.orig),]
    numcol.YXWO <- dim(YXWO.orig)[2]
    y.vect <- YXWO.complete[,1]
    #NB - must specify X.mat as.matrix because otherwise with a one parameter linear predictor
    #ie just the column of 1s for the intercept, X.mat is n x 1 and defaults to vector which does
    #not then work in the matrix multiplication code below 
    X.mat <- as.matrix(YXWO.complete[,(2:(numcol.YXWO-2))])
    weightsvar <- YXWO.complete[,numcol.YXWO-1]
    offsetvar <- YXWO.complete[,numcol.YXWO]
  }
  
  #Offset no weights
  if(is.null(weights)&&!(is.null(offset))){
    YXO.orig <- cbind(y.vect.orig,X.mat.orig,offsetvar.orig)
    YXO.complete <- YXO.orig[stats::complete.cases(YXO.orig),]
    numcol.YXO <- dim(YXO.orig)[2]
    y.vect <- YXO.complete[,1]
    #NB - must specify X.mat as.matrix because otherwise with a one parameter linear predictor
    #ie just the column of 1s for the intercept, X.mat is n x 1 and defaults to vector which does
    #not then work in the matrix multiplication code below 
    X.mat <- as.matrix(YXO.complete[,(2:(numcol.YXO-1))])
    weightsvar <- rep(1,length(y.vect))
    offsetvar <- YXO.complete[,numcol.YXO]
  }
  
  #Weights no offset
  if(!(is.null(weights))&&(is.null(offset))){
    YXW.orig <- cbind(y.vect.orig,X.mat.orig,weightsvar.orig)
    YXW.complete <- YXW.orig[stats::complete.cases(YXW.orig),]
    numcol.YXW <- dim(YXW.orig)[2]
    y.vect <- YXW.complete[,1]
    X.mat <- as.matrix(YXW.complete[,(2:(numcol.YXW-1))])
    weightsvar <- YXW.complete[,numcol.YXW]
    offsetvar <- rep(0,length(y.vect))
  }
  
  #No weights or offset
  if(is.null(weights)&&(is.null(offset))){
    y.vect <- y.vect.orig
    X.mat <- X.mat.orig
    weightsvar <- rep(1,length(y.vect))
    offsetvar <- rep(0,length(y.vect))
  }

  numsubs <- length(y.vect)
  
  #Convert beta.vect from transmittable (character) format to numeric 
  beta.vect.n <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  
  #If an offset is specified, add it directly to the values in the linear predictor
  if(!is.null(offset)){
    lp.vect <- (X.mat %*% beta.vect.n)+offsetvar
  }else{
    lp.vect <- (X.mat %*% beta.vect.n)   
  }
  
  #Use the available functions for family f to generate the components giving the deviance and
  #the working weights for the IRLS algorithm
  mu.vect <- f$linkinv(lp.vect)
  mu.eta.val <- f$mu.eta(lp.vect)
  var.vect <- f$variance(mu.vect)
  
  #If a prior weights vector is specified multiply the working weights by the prior weights 
  if(!is.null(weights)){
    W.vect <- as.vector(mu.eta.val^2/var.vect)
    W.vect <- W.vect*weightsvar
    dev <- sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect)))*weightsvar)
  }else{
    W.vect <- as.vector(mu.eta.val^2/var.vect)
    dev <- sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect))))
  }
  
  #Generate information matrix as XWX  
  WX.mat <- W.vect*X.mat
  info.matrix <- t(X.mat)%*%WX.mat
  
  #Generate score vector as XWz (where z is working response vector on scale of linear predictor)
  #See theoretical basis in the .pdf in RELEVANT.GLM.THEORY directory.
  #Note mu.et.val is first differential of inverse link function (d.mu by d.eta)
  #which is inverse of first diff of link function (g') in thoretical explanation
  
  u.vect <- (y.vect-mu.vect)*1/mu.eta.val
  W.u.mat <- matrix(W.vect*u.vect)
  score.vect <- t(X.mat)%*%W.u.mat

  ##########################
  #BACKUP DISCLOSURE TRAP
  #If y, X or w data are invalid but user has modified clientside
  #function (ds.glm) to circumvent trap, model will get to this point without
  #giving a controlled shut down with a warning about invalid data.
  #So as a safety measure, we will now use the same test that is used to
  #trigger a controlled trap in the clientside function to destroy the
  #score.vector and information.matrix in the study with the problem.
  #So this will make model fail without explanation
  
  #Disclosure code from glmDS1

  errorMessage.combined <- NULL

  dimX <- dim((X.mat))
  
  ##############################################################
  #FIRST TYPE OF DISCLOSURE TRAP - TEST FOR OVERSATURATED MODEL#
  ##############################################################
  glm.saturation.invalid <- 0
  num.p <- dimX[2]
  num.N <- dimX[1]
  
  if(num.p>nfilter.glm*num.N){
    glm.saturation.invalid <- 1
    errorMessage.combined <- c(errorMessage.combined,"ERROR: Model has too many parameters, there is a possible risk of disclosure - please simplify model")
  }
  
  ################################
  #SECOND TYPE OF DISCLOSURE TRAP#
  ################################
  
  #CHECK Y VECTOR VALIDITY
  y.invalid <- 0
  
  #COUNT NUMBER OF UNIQUE NON-MISSING VALUES - DISCLOSURE RISK ONLY ARISES WITH TWO LEVELS
  unique.values.noNA.y <- unique(y.vect[stats::complete.cases(y.vect)])
  
  #IF TWO LEVELS, CHECK WHETHER EITHER LEVEL 0 < n < nfilter.tab
  
  if(length(unique.values.noNA.y)==2){
    tabvar <- table(y.vect)[table(y.vect)>=1]   #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
    min.category <- min(tabvar)
    if(min.category<nfilter.tab){
      y.invalid <- 1
      errorMessage.combined <- c(errorMessage.combined,"ERROR: y vector is binary with one category less than filter threshold for table cell size")
    }
  }
  
  
  #CHECK X MATRIX VALIDITY 
  #Check no dichotomous X vectors with between 1 and filter.threshold 
  #observations at either level 
  dimX <- dim((X.mat))
  
  num.Xpar <- dimX[2]
  
  Xpar.invalid <- rep(0,num.Xpar)
  x.invalid <- 0
  
  for(pj in 1:num.Xpar){
    unique.values.noNA <- unique((X.mat[,pj])[stats::complete.cases(X.mat[,pj])]) 
    if(length(unique.values.noNA)==2){
      tabvar <- table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
      min.category <- min(tabvar)
      if(min.category<nfilter.tab){
        Xpar.invalid[pj] <- 1
	x.invalid <- 1
      }
    }
  }

  if(x.invalid==1){
    errorMessage.combined <- c(errorMessage.combined, "ERROR: at least one column in X matrix is binary with one category less than filter threshold for table cell size")
  }

  #CHECK W VECTOR VALIDITY
  w.invalid <- 0
  
  #Keep same object name as in glmDS1
  w.vect <- weightsvar
  
  unique.values.noNA.w <- unique(w.vect[stats::complete.cases(w.vect)])
  
  if(length(unique.values.noNA.w)==2){
    tabvar <- table(w.vect)[table(w.vect)>=1]   #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
    min.category <- min(tabvar)
    if(min.category<nfilter.tab){
      w.invalid <- 1
      errorMessage.combined <- c(errorMessage.combined,"ERROR: w vector is binary with one category less than filter threshold for table cell size")
    }
  }
  
  #Check o vector validity
  o.invalid <- 0

  #if(!is.null(offsetvar))
  #{	
	#Keep vector name consistent
	o.vect <- offsetvar

	unique.values.noNA.o <- unique(o.vect[stats::complete.cases(o.vect)])

	if(length(unique.values.noNA.o)==2){
	  tabvar <- table(o.vect)[table(o.vect)>=1]   #tabvar counts n in all categories with at least one observation
	  min.category <- min(tabvar)
	  if(min.category<nfilter.tab){
            o.invalid <- 1
	    errorMessage.combined<-c(errorMessage.combined,"ERROR: offset vector is binary with one category less than filter threshold for table cell size")
          }
	}
#}
	
  disclosure.risk<-0
 
  ########################################################################
  #If there is a disclosure risk DESTROY the info.matrix and score.vector#
  ########################################################################
  
  if(y.invalid>0||w.invalid>0||o.invalid>0||sum(Xpar.invalid)>0||glm.saturation.invalid>0){
    info.matrix <- NA
    score.vector <- NA
    disclosure.risk <- 1
    errorMessage.combined <- c(errorMessage.combined,"MODEL FAILED: model or data invalid, info.matrix and score.vector destroyed")
  }else{
    errorMessage.combined <- "No errors"
  }
  
  return(list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev,
              Nvalid=Nvalid,Nmissing=Nmissing,Ntotal=Ntotal,disclosure.risk=disclosure.risk,
              errorMessage2=errorMessage.combined))  
 
}
# AGGREGATE FUNCTION
# glmDS2
