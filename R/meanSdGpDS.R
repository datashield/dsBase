#' 
#' @title MeanSdGpDS
#' @description Server-side function called by ds.meanSdGp 
#' @details Computes the mean and standard deviation across groups defined by one
#' factor
#' @param X a client-side supplied character string identifying the variable for which
#' means/SDs are to be calculated
#' @param INDEX a client-side supplied character string identifying the factor across
#' which means/SDs are to be calculated
#' @author Burton PR
#' 
#' @return List with results from the group statistics
#' @export
#'
meanSdGpDS <- function (X, INDEX){
  
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  FUN.mean <- function(x) {mean(x,na.rm=TRUE)}
  FUN.var <- function(x)  {stats::var(x,na.rm=TRUE)}
  
  #Strip missings from both X and INDEX
  analysis.matrix<-cbind(X,INDEX)
  
  data.complete<-stats::complete.cases(analysis.matrix)
  
  Ntotal<-dim(analysis.matrix)[1]
  Nmissing<-sum(!data.complete)
  Nvalid<-sum(data.complete)
  
  simplify<-TRUE
  
  analysis.matrix.no.miss<-analysis.matrix[data.complete,]
  nv<-dim(analysis.matrix)[2]
  
  X<-as.vector(analysis.matrix.no.miss[,1])
  INDEX<-analysis.matrix.no.miss[,2:nv]
  
  
  if (!is.list(INDEX)) 
    INDEX <- list(INDEX)
  nI <- length(INDEX)
  if (!nI) 
    stop("'INDEX' is of length zero")
  namelist <- vector("list", nI)
  names(namelist) <- names(INDEX)
  extent <- integer(nI)
  nx <- length(X)
  one <- 1L
  group <- rep.int(one, nx)
  ngroup <- one
  for (i in seq_along(INDEX)) {
    index <- as.factor(INDEX[[i]])
    if (length(index) != nx) 
      stop("arguments must have same length")
    namelist[[i]] <- levels(index)
    extent[i] <- nlevels(index)
    group <- group + ngroup * (as.integer(index) - one)
    ngroup <- ngroup * nlevels(index)
  }
  #    if (is.null(FUN.mean)) 
  #        return(group)
  
  #CALCULATE GROUP MEANS
  ans <- lapply(X = split(X, group), FUN = FUN.mean)
  index <- as.integer(names(ans))
  if (simplify && all(unlist(lapply(ans, length)) == 1L)) {
    ansmat <- array(dim = extent, dimnames = namelist)
    ans <- unlist(ans, recursive = FALSE)
  }
  else {
    ansmat <- array(vector("list", prod(extent)), dim = extent, 
                    dimnames = namelist)
  }
  if (length(index)) {
    names(ans) <- NULL
    ansmat[index] <- ans
  }
  ansmat.mean<-ansmat
  
  #CALCULATE GROUP SDs
  ans <- lapply(X = split(X, group), FUN = FUN.var)
  index <- as.integer(names(ans))
  if (simplify && all(unlist(lapply(ans, length)) == 1L)) {
    ansmat <- array(dim = extent, dimnames = namelist)
    ans <- unlist(ans, recursive = FALSE)
  }
  else {
    ansmat <- array(vector("list", prod(extent)), dim = extent, 
                    dimnames = namelist)
  }
  if (length(index)) {
    names(ans) <- NULL
    ansmat[index] <- ans
  }
  ansmat.sd<-sqrt(ansmat)
  
  
  #CALCULATE GROUP SIZES AND CHECK VALID
  
  ansmat.count<-table(group)
  
  # Set filter for cell sizes that are too small
  # the minimum number of observations that are allowed (the below function gets the value from opal)
  any.invalid.cell<-(sum(ansmat.count<nfilter.tab&ansmat.count>0)>=1)
  if(!any.invalid.cell)
  {
    table.valid<-TRUE
    cell.count.warning<-paste0("All tables valid") 
    result<-list(table.valid,ansmat.mean,ansmat.sd,ansmat.count,Nvalid,Nmissing,Ntotal,cell.count.warning)
    names(result)<-list("Table_valid","Mean_gp","StDev_gp", "N_gp","Nvalid","Nmissing","Ntotal","Message")
    return(result)
  }
  
  if(any.invalid.cell)
  {
    table.valid<-FALSE
    cell.count.warning<-paste0("At least one group has between 1 and ", nfilter.tab-1, " observations. Please change groups") 
    result<-list(table.valid,Nvalid,Nmissing,Ntotal,cell.count.warning)
    names(result)<-list("Table_valid","Nvalid","Nmissing","Ntotal","Warning")
    return(result)
  }
  
}
#AGGREGATE function
# meanSdGpDS
