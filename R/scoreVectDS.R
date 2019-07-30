#'
#' @title Generates the score vector and information matrix
#' @description This function is called by the client function 'ds.gee'
#' to produced the score vector and information matrix.
#' @details the score vector and information matrix are calculated according to 
#' the correlation structure.
#' @param data the input dataframe which contains the variable specified in the formula.
#' @param formula a regression formula.
#' @param family the link function for the regression.
#' @param clusterID the name of the column that holds the cluster IDs.
#' @param corstr the correlation structure.
#' @param alpha the parameter alpha
#' @param phi the parameter alpha
#' @param startBetas a character, the starting values concatenated by comma
#' @param zcor the user defined matrix user if the correlation structure is 'fixed'.
#' @return a list
#' @author Gaye, A.; Jones EM.
#' @export
#' 
scoreVectDS <- function(data, formula, family, clusterID, corstr, alpha, phi, startBetas, zcor=NULL){
  
  input.table <-  data[order(data[,clusterID]), ]
  id.indx <- which(colnames(input.table) == clusterID)
  id <- input.table[,id.indx]
  startBetas <- as.numeric(unlist(strsplit(startBetas,split=',')))
  
  # THESE TWO LINES GET THE 'X' and "Y" WE WERE GETTING THROUGH ONE the geeglm FUNCTION OF 'GEEPACK'
  X.mat <- model.matrix(formula, data)
  outcomeName <- all.vars(formula)[1]
  y.vect <- as.numeric(as.character(data[,which(colnames(data) == outcomeName)]))
  
  # NUMBER OF BETA PARAMETERS 
  npara <- dim(X.mat)[2]
  
  # NUMBER OF OBSERVATIONS (SUBJECTS)
  N <- length(id)
  
  # GET LAGGED AND ITERATED DIFFERENCES OF THE VECTOR IDs (BY DEFAULT'LAG'=1 AND 'ORDER OF DIFFERENCE' = 1)
  clusnew <- c(which(diff(as.numeric(id)) != 0), length(id))
  clusz <- c(clusnew[1], diff(clusnew))
  
  # NUMBER OF CLUSTERS (i.e. NUMBER OF SUBJECTS)
  N.clus <- length(clusz)
  
  # ESTIMATED RESPONSE
  lp.vect <- X.mat%*%startBetas
  
  # VALUES FOR THE INVERSE LINK FUNCTION AND THE RELATED MEAN AND VARIANCE
  if(family == "binomial"){ famlink <- binomial(link = "logit") }
  if(family == "gaussian"){ famlink <- gaussian(link = "identity") }
  if(family == "Gamma"){ famlink <- Gamma(link = "inverse") }
  if(family == "poisson"){ famlink <- poisson(link = "log") }
  f <- famlink
  mu.vect <- f$linkinv(lp.vect)
  var.vect <- f$variance(mu.vect)
  
  
  # ALPHA (INTRACLASS CORRELATION VALUE) PARAMETER IN CORRELATION STRUCTURE MUST BE < 1 IN ABSOLUTE VALUE
  # RE-SET IT TO -1 OR 1 IF ITS ABSOLUTE IS 'SLIGHTLY' GREATER THAN 1
  alpha[which(alpha >= 1 & alpha < 1.01)] <- 0.999999
  alpha[which(alpha <= -1 & alpha > -1.01)] <- -0.999999
  
  # LOAD THE 'nlme' PACKAGE TO USE FUNCTIONS TO CREATE CORRELATION STRUCTURES
  library("nlme")
  
  # MATRIX IF THE CORRELATION STRUCTURE IS 'AUTOREGRESSIVE AR (1)'
  if(corstr == "ar1"){
    # THE ORDER OF THE OBSERVATIONS WITHIN A GROUP IS USED AS POSITION VARIABLE 
    # (THE FIRST ARGUMENT OF THE 'FORM' PARAMETER) AND THE IDS ARE USED AS GROUPING PARAMETER 
    R.mat.AR1 <- corAR1(alpha, form = ~ 1 | id)
    R.mat.AR1.i <- Initialize(R.mat.AR1, data=input.table)
    list.of.matrices <- corMatrix(R.mat.AR1.i)
  }
  
  # MATRIX IF THE CORRELATION STRUCTURE IS 'EXCHANGEABLE'
  if(corstr == "exchangeable"){
    R.mat.EXCH <- corCompSymm(alpha, form = ~ 1 | id)
    R.mat.EXCH.i <- Initialize(R.mat.EXCH, data=input.table)
    list.of.matrices <- corMatrix(R.mat.EXCH.i)
  }
  
  # MATRIX IF THE CORRELATION STRUCTURE IS 'INDEPENDENT'
  if(corstr == "independence"){
    R.mat.INDP <- corCompSymm(0, form = ~ 1 | id)
    R.mat.INDP.i <- Initialize(R.mat.INDP, data=input.table)
    list.of.matrices <- corMatrix(R.mat.INDP.i)
    
  }
  
  # MATRIX IF THE CORRELATION STRUCTURE IS 'UNSTRUCTERED'
  if(corstr == "unstructured"){
    mt.temp <- matrix(0, nrow=max(table(id)), ncol=max(table(id)))
    num.alpha.vals <- length(mt.temp[col(mt.temp) < row(mt.temp)])
    R.mat.unstr <- corSymm(alpha[1:num.alpha.vals], form = ~ 1 | id)
    R.mat.unstr.i <- Initialize(R.mat.unstr, data=input.table)
    list.of.matrices <- corMatrix(R.mat.unstr.i)
  }
  
  # MATRIX IF THE CORRELATION STRUCTURE IS 'FIXED' (USER DEFINED)
  if(corstr == "fixed"){
    low.diag.elts <- zcor[col(zcor) < row(zcor)]
    R.mat.userdef <- corSymm(low.diag.elts, form = ~ 1 | id)
    R.mat.userdef.i <- Initialize(R.mat.userdef, data=input.table)
    list.of.matrices <- corMatrix(R.mat.userdef.i)
  }
  
  R.mat <- list.of.matrices

  # CREATING THE A MATRIX (LIANG AND ZEGER)
  A.mat <- vector("list", N.clus)
  A.mat[[1]] <- phi^(-1)*diag(var.vect[1:clusnew[1]])
  for(i in 2:N.clus){
    A.mat[[i]] <- phi^(-1)*diag(var.vect[(clusnew[i-1]+1):clusnew[i]]) 
  }
  cl <- class(id)
  
  
   # CREATING THE V MATRIX - ESTIMATE OF THE WORKING CORRELATION MATRIX (LIANG AND ZEGER)
   V.mat <- vector("list", N.clus)
   for(i in 1:N.clus){
     V.mat[[i]] <- phi*(sqrt(A.mat[[i]])%*%R.mat[[i]]%*%sqrt(A.mat[[i]]))  
   }
  
  # FAMILY-SPECIFIC FUNCTIONS TO HELP CALCULATE BETA
  deriv.vect <- f$mu.eta(lp.vect)
  
  if(f$family=="gaussian"){
    der.vect <- rep(1,N)
  }
  
  if(f$family=="poisson") {
    der.vect <- mu.vect^(-1)
  }
  
  if(f$family=="binomial"){
    der.vect <- 1/(mu.vect*(1-mu.vect))
  }
  
  if(f$family=="Gamma"){
    der.vect <- 1/(mu.vect^2)
  }
  
  if(f$family=="inverse.gaussian"){
    der.vect <- 1/(mu.vect^3)
  }
  
  # DELTA MATRIX, LIANG AND ZEGER. THIS IS AN IDENTITY MATRIX IF USING CANONICAL LINK FOR A FAMILY
  Delta.vec<-deriv.vect*der.vect
  Delta.mat<-vector("list",N.clus)
  Delta.mat[[1]] <- diag(Delta.vec[1:clusnew[1]])
  for(i in 2:N.clus){
    Delta.mat[[i]] <- diag(Delta.vec[(clusnew[i-1]+1):clusnew[i]])
  }
  
  # D MATRIX OF PARTIAL DERIVATIVES, LIANG AND ZEGER
  D.mat <- vector("list", N.clus)
  D.mat[[1]] <- t(A.mat[[1]])%*%Delta.mat[[1]]%*%X.mat[1:clusnew[1],]
  for(i in 2:N.clus){
    D.mat[[i]] <- t(A.mat[[i]])%*%Delta.mat[[i]]%*%X.mat[ (clusnew[i-1]+1):clusnew[i],]
  }
  
  # CALULATING ALL CLUSTER-SPECIFIC INFORMATION MATRICES 
  I.mat<-vector("list", N.clus)
  for(i in 1:N.clus){
    I.mat[[i]] <- t(D.mat[[i]])%*%solve(V.mat[[i]])%*%D.mat[[i]]
  }
  
  # SUMMING ALL CLUSTER-SPECIFIC INFORMATION MATRICES
  infomatrix <- matrix(rep(0,npara^2), ncol=npara)
  for (i in 1:N.clus){
    infomatrix <- infomatrix+I.mat[[i]]
  }
  
  # CALULATING ALL CLUSTER-SPECIFIC SCORE VECTORS
  s.vec <- vector("list", N.clus)
  s.vec[[1]] <- t(D.mat[[1]])%*%solve(V.mat[[1]])%*%(y.vect[1:clusnew[1]]-mu.vect[1:clusnew[1]])
  for(i in 2:N.clus){
    s.vec[[i]] <- t(D.mat[[i]])%*%solve(V.mat[[i]])%*%(y.vect[(clusnew[i-1]+1):clusnew[i]]-mu.vect[(clusnew[i-1]+1):clusnew[i]])
  }
  
  # SUMMING ALL CLUSTER-SPECIFIC SCORE VECTORS
  score <- c(rep(0, npara))
  for (i in 1:N.clus){
    score <- score+s.vec[[i]]
  }
  
  # J.matrices needed to calculate standard error of estimates
  J1 <- vector("list", N.clus)
  J1[[1]] <- t(D.mat[[1]])%*%solve(V.mat[[1]])%*%(y.vect[1:clusnew[1]]-mu.vect[1:clusnew[1]])%*%t(y.vect[1:clusnew[1]]-mu.vect[1:clusnew[1]])%*%solve(V.mat[[1]])%*%D.mat[[1]]
  for(i in 2:N.clus){
    J1[[i]] <- t(D.mat[[i]])%*%solve(V.mat[[i]])%*%(y.vect[(clusnew[i-1]+1):clusnew[i]]-
                                                      mu.vect[(clusnew[i-1]+1):clusnew[i]])%*%t(y.vect[(clusnew[i-1]+1):clusnew[i]]
                                                                                                -mu.vect[(clusnew[i-1]+1):clusnew[i]])%*%solve(V.mat[[i]])%*%D.mat[[i]]
  }
  J.matrix <- matrix(rep(0,npara^2), ncol=npara)
  for (i in 1:N.clus){
    J.matrix <- J.matrix + J1[[i]]
  }
  
  # OUTPUT
  return(list(score.vector=score, info.matrix=infomatrix, J.matrix=J.matrix))
 
}