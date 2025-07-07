#'
#' @title testObjExistsDS
#' @description The server-side function called by ds.testObjExists
#' @details Tests whether a given object exists in
#' all sources. It is called at the end of all
#' recently written assign functions to check the new (assigned) object has been
#' created in all sources
#' @param test.obj.name a client-side provided character string specifying the variable
#' whose presence is to be tested in each data source
#' 
#' @author Burton PR
#' 
#' @return List with `test.obj.exists` and `test.obj.class`
#' @export
#'
testObjExistsDS <- function(test.obj.name=NULL){

  test.obj.exists <- FALSE
  test.obj.class <- NULL

  #Restrict tests to settings where name does exist to avoid error termination

  if(exists(test.obj.name, envir = parent.frame())){
    test.obj.exists <- TRUE
    test.obj <- get(test.obj.name, envir = parent.frame())
    test.obj.class <- class(test.obj)
  }

  return(list(test.obj.exists=test.obj.exists,test.obj.class=test.obj.class))

}
#AGGREGATE FUNCTION
# testObjExistsDS
