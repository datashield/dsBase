#' 
#' @title BooleDS
#' @description Converts the individual elements of a vector or other object into
#' Boolean indicators.
#' @details The function converts the input vector into Boolean indicators.
#' @param V1.name A character string specifying the name of the vector to which the
#' Boolean operator is to be applied
#' @param V2.name A character string specifying the name of the vector or scalar to
#' which <V1> is to be compared.
#' @param Boolean.operator.n An integer value (1 to 6) providing a numeric coding for
#' the character string
#' specifying one of six possible Boolean operators: '==', '!=', '>', '>=','<', '<='
#' that could legally be passed
#' from client to server via DataSHIELD parser
#' @param numeric.output a TRUE/FALSE indicator defaulting to TRUE determining whether
#' the final output variable
#' should be of class numeric (1/0) or class logical (TRUE/FALSE).
#' @param na.assign.text A character string taking values 'NA', '1' or '0'. If 'NA'
#' then any NA values in the input vector remain as NAs in the output vector. If '1' 
#' or '0' NA values in the input vector are all converted to 1 or 0 respectively.
#' @return the levels of the input variable.
#' @author DataSHIELD Development Team
#' @export
#'
BooleDS <- function(V1.name=NULL, V2.name=NULL, Boolean.operator.n=NULL, na.assign.text, numeric.output=TRUE){

  # Check Permissive Privacy Control Level.
  dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'banana'))

  V1 <- eval(parse(text=V1.name), envir = parent.frame())
  V2 <- eval(parse(text=V2.name), envir = parent.frame())
  
  if(is.character(V1)){
    studysideMessage <- "FAILED: V1 is character, please convert to numeric, factor or logical before running Boole"
    stop(studysideMessage, call. = FALSE)
  }
  
  if(is.character(V2)){
    studysideMessage <- "FAILED: V2 is character, please convert to numeric, factor or logical before running Boole"
    stop(studysideMessage, call. = FALSE)
  }

  V1.length <- length(V1)   
  V2.length <- length(V2)

  if(!((V1.length == V2.length) | (V2.length==1))){
    studysideMessage <- "FAILED: V2 must either be of length one or of length equal to V1"
    stop(studysideMessage, call. = FALSE)
  }

  if(!(Boolean.operator.n %in% c(1,2,3,4,5,6))){
    studysideMessage <- "FAILED: Boolean.operator specified incorrectly. Must be: '==', '!=', '<', '<=', '>' or '>='"
    stop(studysideMessage, call. = FALSE)
  }

  Boolean.operator <- " "
  if(Boolean.operator.n==1) Boolean.operator <- "=="
  if(Boolean.operator.n==2) Boolean.operator <- "!="
  if(Boolean.operator.n==3) Boolean.operator <- "<"
  if(Boolean.operator.n==4) Boolean.operator <- "<="
  if(Boolean.operator.n==5) Boolean.operator <- ">"
  if(Boolean.operator.n==6) Boolean.operator <- ">="

  # APPLY BOOLEAN OPERATOR SPECIFIED
  Boolean.indicator <- integer(length=V1.length)

  # EVALUATE DIFFERENTLY IF V2 IS SAME LENGTH AS V1 OR OF LENGTH 1
  if(V2.length==V1.length){
    for(j in 1:V1.length){
      command.text <- paste0(V1.name,"[",j,"] ",Boolean.operator," ",V2.name,"[",j,"]")
      Boolean.indicator[j]<-eval(parse(text=command.text), envir = parent.frame())*1
    }
  }

  if(V2.length==1){
    for(j in 1:V1.length){
      command.text<-paste0(V1.name,"[",j,"] ",Boolean.operator," ",V2.name)
      Boolean.indicator[j]<-eval(parse(text=command.text), envir = parent.frame())*1
    }
  }

  # BY DEFAULT NAs REMAIN AS NAs BUT IF YOU WANT TO YOU CAN FORCE THEM TO 1 OR 0 USING <na.assign.text> ARGUMENT
  if(na.assign.text=="1"){
    Boolean.indicator[is.na(Boolean.indicator)==1]<-1
  }

  if(na.assign.text=="0"){
    Boolean.indicator[is.na(Boolean.indicator)==1]<-0
  }

  outobj.b <- as.logical(Boolean.indicator)
  outobj <- Boolean.indicator

  if(numeric.output==TRUE){
    Boole.obj <- outobj
  }else{
    Boole.obj <- outobj.b
  }

  return(Boole.obj)
}
# ASSIGN FUNCTION
# BooleDS
