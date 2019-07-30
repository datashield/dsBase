#'
#' @title Computes the parameters alpha and phi
#' @description This function is called by the client function 'ds.gee'
#' to calculate the parameters alpha and phi.
#' @details the parameters are calculated according to the correlation structure.
#' @param data the input dataframe which contains the variable specified in the 
#' formula.
#' @param formula a regression formula.
#' @param family an object of class Family.
#' @param clusterID the name of the column that holds the cluster IDs.
#' @param corstr the correlation structure.
#' @param startBetas a character, the starting values concatenated by comma
#' because it is not possible to use 'c()' in aggregate functions.
#' @return a list
#' @author Gaye, A.; Jones EM.
#' @export
#' 
alphaPhiDS <- function (data, formula, family, clusterID, corstr, startBetas){
  
  input.table <-  data[order(data[,clusterID]), ]
  id.indx <- which(colnames(input.table) == clusterID)
  id <- input.table[,id.indx]
  startBetas  <- as.numeric(unlist(strsplit(startBetas,split=',')))
  
  # these two lines the 'X' and "Y" obtained through the 'geeglm'function of the package 'geepack'
  X.mat <- model.matrix(formula, data)
  Y.vec <- as.vector(model.response(model.frame(formula, data), type="numeric"))
  
  npara <- dim(X.mat)[2]
  N <- length(id)
  clusnew <- c(which(diff(as.numeric(id)) != 0), length(id))
  clusz <- c(clusnew[1], diff(clusnew))
  N.clus <- length(clusz)
  maxclsz <- max(clusz)

  # extracting family and similar information
  if(family == "binomial"){ famlink <- binomial(link = "logit") }
  if(family == "gaussian"){ famlink <- gaussian(link = "identity") }
  if(family == "Gamma"){ famlink <- Gamma(link = "inverse") }
  if(family == "poisson"){ famlink <- poisson(link = "log") }
  f <- famlink
  LINKS <- c("identity", "logit", "probit", "cloglog", "log", "inverse", "fisherz", "lwybc2", "lwylog")
  VARIANCES <- c("gaussian", "binomial", "poisson", "Gamma")
  mean.link <- f$link
  variance <- f$variance
  mean.link.v <- pmatch(mean.link, LINKS, -1, TRUE)
  mu <- quasi(LINKS[mean.link.v])$linkinv(X.mat%*%startBetas)
  
  # extracting summary statistics to calculate phi
  pr2 <- (as.numeric(Y.vec) - as.numeric(mu))^2/f$variance(mu)
  pr <-(as.numeric(Y.vec) - as.numeric(mu))/sqrt(f$variance(mu))
  sum_p <- sum(pr2)
  # phi according to Zeger and Liang
  phi <- (N-npara)^{-1}*sum_p						 
  
  # preparing to estimate alpha
  pearson <- matrix(pr, ncol=1)
  mat.clus <- vector("list", N.clus)
  mat.clus[[1]] <- pearson[1:clusnew[1]]%*%t(pearson[1:clusnew[1]])
  for(i in 2:N.clus){
    mat.clus[[i]] <- pearson[(clusnew[i-1]+1):clusnew[i]]%*%t(pearson[(clusnew[i-1]+1):clusnew[i]])
  }

  # for 'exchangeable' correlation structure
  if(corstr=="exchangeable"){
    clus.sq <- vector("list", N.clus)
    prod.clus <- rep(0,N.clus)
    clus.sq[[1]] <- sum(pearson[1:clusnew[1]]^2)
    prod.clus[[1]] < -sum(mat.clus[[1]])-clus.sq[[1]]
    for(i in 2:N.clus){
      clus.sq[[i]] <- sum(pearson[(clusnew[i-1]+1):clusnew[i]]^2)
      prod.clus[[i]] <- sum(mat.clus[[i]])-clus.sq[[i]]
    }
    temp <- sum(prod.clus)
  }
  
  # for 'untructure' correlation structure
  if(corstr=="unstructured"){
    temp <- array(rep(0,(N.clus*max(clusz)^2)), dim=c(N.clus,max(clusz),max(clusz)))
    alpha.unstructured <- matrix(0, nrow=max(clusz), ncol=max(clusz))
    pearson.unstr <- vector("list", N.clus)
    pearson.unstr[[1]] <- matrix(c(pr[1:clusnew[1]]), ncol=1)
    for(i in 2:N.clus){
      pearson.unstr[[i]] <- matrix(c(pr[(clusnew[i-1]+1):clusnew[i]]), ncol=1)
    }
    for(i in 1:N.clus){
      for(j in 1:clusz[i]){
        for(m in 1:clusz[i]){
          temp[i,j,m] <- pearson.unstr[[i]][j]
          alpha.unstructured[j,m] <- sum(temp[,j,m])/((N.clus-npara)*phi)
        }
      }
    }
    alpha.unstructured <- alpha.unstructured[col(alpha.unstructured) < row(alpha.unstructured)]
  }
  
  # for 'ar1' correlation structure
  if(corstr=="ar1"){
    component <- matrix(rep(0,(N.clus*(max(clusz)-1))), nrow=N.clus)
    for(i in 1:N.clus){
      for(j in 1:(clusz[i]-1)){
        component[i,j] <- mat.clus[[i]][j,j+1]
      }
    }
    temp <- sum(component)
  }

  # set alpha according to the specified correlation structure
  if(corstr=="exchangeable"){
    M <- phi*sum(clusz*(clusz-1))-phi*npara
    alpha <- temp/M 
    M_study <- sum(clusz*(clusz-1))
    alphaM <- M*alpha
  }
  
  if(corstr=="ar1"){
    M <- phi* sum((clusz-1))-phi*npara
    alpha <- temp/M 
    M_study <- sum((clusz-1))
    alphaM <- M*alpha
  }
  
  if(corstr=="unstructured"){
    M_study <--999;
    alphaM <-(N.clus-npara)*phi*alpha.unstructured
  }
  
  if(corstr=="independence"|corstr=="fixed"){
    M_study <- -999;
    alphaM <- -999
  }
  
  output <- list(N=N, npara=npara, M_study=M_study, alphaM=alphaM, sum_p=sum_p) 
  return(output)
}
