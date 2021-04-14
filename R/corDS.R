#'
#' @title Computes the sum of each variable and the sum of products for each pair of variables
#' @description This function computes the sum of each vector of variable and the sum of the products
#' of each two variables (i.e. the scalar product of each two vectors).
#' @details computes the sum of each vector of variable and the sum of the products of each two
#' variables
#' @param x a character, the name of a vector, matrix or dataframe of variables(s) for which the
#' correlation(s) is (are) going to calculated for.
#' @param y NULL (default) or the name of a vector, matrix or dataframe with compatible dimensions to x.
#' @return a list that includes a matrix with elements the sum of products between each two variables,
#' a matrix with elements the sum of the values of each variable, a matrix with elements the number of
#' complete cases in each pair of variables, a list with the number of missing values in each variable
#' separately (columnwise) and the number of missing values casewise, and a vector with elements the
#' sum of squares of each variable. The first disclosure control checks that the number of variables is
#' not bigger than a percentage of the individual-level records (the allowed percentage is pre-specified
#' by the 'nfilter.glm'). The second disclosure control checks that none of them is dichotomous with a
#' level having fewer counts than the pre-specified 'nfilter.tab' threshold.
#' @author Paul Burton, and Demetris Avraam for DataSHIELD Development Team
#' @export
#'
corDS <- function(x=NULL, y=NULL){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  nfilter.glm <- as.numeric(thr$nfilter.glm)
  #############################################################
  
  x.val <- eval(parse(text=x), envir = parent.frame())
  if (!is.null(y)){
    y.val <- eval(parse(text=y), envir = parent.frame())
  }
  else{
    y.val <- NULL
  }
  
  # create a data frame for the variables
  if (is.null(y.val)){
    dataframe <- as.data.frame(x.val)
  }else{
    dataframe <- as.data.frame(cbind(x.val,y.val))
  }
  
  # names of the variables
  cls <- colnames(dataframe)
  
  # number of the input variables
  N.vars <- ncol(dataframe)
  
  ######################
  # DISCLOSURE CONTROLS
  ######################
  
  ##############################################################
  # FIRST TYPE OF DISCLOSURE TRAP - TEST FOR OVERSATURATION
  # TEST AGAINST nfilter.glm								
  ##############################################################
  
  varcov.saturation.invalid <- 0
  
  if(N.vars > (nfilter.glm * nrow(dataframe))){
    
    varcov.saturation.invalid <- 1
    
    studysideMessage <- "ERROR: The ratio of the number of variables over the number of individual-level
                          records exceeds the allowed threshold, there is a possible risk of disclosure"
    stop(studysideMessage, call. = FALSE)
    
  }
  
  # CHECK X MATRIX VALIDITY
  # Check no dichotomous X vectors with between 1 and filter.threshold
  # observations at either level
  
  X.mat <- as.matrix(dataframe)
  
  dimX <- dim(X.mat)
  
  num.Xpar <- dimX[2]
  
  Xpar.invalid <- rep(0, num.Xpar)
  
  for(pj in 1:num.Xpar){
    unique.values.noNA <- unique((X.mat[,pj])[stats::complete.cases(X.mat[,pj])])
    if(length(unique.values.noNA)==2){
      tabvar <- table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
      min.category <- min(tabvar)
      if(min.category < nfilter.tab){
        Xpar.invalid[pj] <- 1
      }
    }
  }
  
  # if any of the vectors in X matrix is invalid then the function returns an error
  
  if(is.element('1', Xpar.invalid)==TRUE & varcov.saturation.invalid==0){
    
    studysideMessage <- "ERROR: at least one variable is binary with one category less than the filter threshold for table cell size"
    stop(studysideMessage, call. = FALSE)

  }
  
  # if all vectors in X matrix are valid then the output matrices are calculated
  
  if(is.element('1', Xpar.invalid)==FALSE & varcov.saturation.invalid==0){
    
      # calculate the number of NAs in each variable separately
      column.NAs <- matrix(ncol=N.vars, nrow=1)
      colnames(column.NAs) <- cls
      for(i in 1:N.vars){
        column.NAs[1,i] <- length(dataframe[,i])-length(dataframe[stats::complete.cases(dataframe[,i]),i])
      }
      
      # remove any rows from the dataframe that include NAs
      casewise.dataframe <- dataframe[stats::complete.cases(dataframe),]
      
      # calculate the number of NAs casewise
      casewise.NAs.all <- dim(dataframe)[1]-dim(casewise.dataframe)[1]
      casewise.NAs <- matrix(casewise.NAs.all, ncol=N.vars, nrow=N.vars)
      rownames(casewise.NAs) <- cls
      colnames(casewise.NAs) <- cls
      
      # counts for NAs to be returned to the client:
      # This is a list with (a) a vector with the number of NAs in each variable (i.e. in each column)
      # separately and (b) the number of NAs casewise (i.e. the number of rows deleted from the input dataframe
      # which are the rows that at least one of their cells include a missing value)
      na.counts <- list(column.NAs, casewise.NAs)
      names(na.counts) <- list(paste0("Number of NAs in each column"), paste0("Number of NAs casewise"))
      
      # A matrix with elements the sum of products between each two variables
      sums.of.products <- matrix(ncol=N.vars, nrow=N.vars)
      rownames(sums.of.products) <- cls
      colnames(sums.of.products) <- cls
      for(m in 1:N.vars){
        for(p in 1:N.vars){
          sums.of.products[m,p] <- sum(as.numeric(as.character(casewise.dataframe[,m]))*as.numeric(as.character(casewise.dataframe[,p])))
        }
      }
      
      # A matrix with elements the sum of each variable
      sums <- matrix(ncol=N.vars, nrow=N.vars)
      rownames(sums) <- cls
      colnames(sums) <- cls
      for(m in 1:N.vars){
        for(p in 1:N.vars){
          sums[m,p] <- sum(as.numeric(as.character(casewise.dataframe[,m])))
        }  
      }
      
      # A matrix with elements the sum of squares of each variable after removing missing values casewise
      sums.of.squares <- matrix(ncol=N.vars, nrow=N.vars)
      rownames(sums.of.squares) <- cls
      colnames(sums.of.squares) <- cls
      for(m in 1:N.vars){
        for(p in 1:N.vars){
          sums.of.squares[m,p] <- sum(as.numeric(as.character(casewise.dataframe[,m]))*as.numeric(as.character(casewise.dataframe[,m])))
        }
      }
      
      # A natrix with elements the number of complete cases casewise
      complete.counts <- matrix(dim(casewise.dataframe)[1], ncol=N.vars, nrow=N.vars)
      rownames(complete.counts) <- cls
      colnames(complete.counts) <- cls
    
  }
  
  return(list(sums.of.products=sums.of.products, sums=sums, complete.counts=complete.counts, na.counts=na.counts, sums.of.squares=sums.of.squares))
  
}
# AGGREGATE FUNCTION
# corDS
