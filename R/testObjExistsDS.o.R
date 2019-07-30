#' 
#' @title testObjExistsDS.o
#' @description The serverside function called by ds.testObjExists.o
#' @details Tests whether a given object exists in
#' all sources. It is called at the end of all
#' recently written assign functions to check the new (assigned) object has been
#' created in all sources
#' @param test.obj.name a clientside provided character string specifying the variable
#' whose presence is to be tested in each data source
#' @author Burton PR
#' @export
#'
testObjExistsDS.o <- function(test.obj.name=NULL){
  
  test.obj.exists <- FALSE
  test.obj.class <- NULL
  
  #Restrict tests to settings where name does exist to avoid error termination
  
  if(exists(test.obj.name)){
    test.obj.exists <- TRUE
    test.obj <- eval(parse(text=test.obj.name))
    test.obj.class <- class(test.obj)
  }
  
  return(list(test.obj.exists=test.obj.exists,test.obj.class=test.obj.class))
  
}
#AGGREGATE FUNCTION
# testObjExistsDS.o
