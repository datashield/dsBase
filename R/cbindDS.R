#'
#' @title Combines objects by columns
#' @description This function is similar to the R base function 'cbind'.
#' @details Unlike the R base function 'cbind' this function' output is a
#' dataframe as that is the type mainly used in DataSHIELD.
#' @param objs a list of object to combine
#' @return a dataframe
#' @author Gaye, A.
#' @export
#' 
#'
cbindDS <- function (objs) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  mtx <-  eval(parse(text=objs[[1]]))
  for(i in 2:length(objs)){
    oo <- eval(parse(text=objs[[i]]))
    mtx <- cbind(mtx, oo)
  }
  
  # convert the matrix into a dataframe
  dt <- as.data.frame(mtx)
  
  # check if the resulting dataframe is valid and output accordingly
  if(dim(dt)[1] < nfilter){
    return(NULL)
  }else{
    return(dt)
  }

}
