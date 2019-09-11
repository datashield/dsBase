#' @title unListDS a serverside aggregate function called by ds.unList
#' @description Coerces an R object back from a list towards
#' the class it was before being coerced to a list
#' @details Unlike most other class coercing functions this is
#' an aggregate function rather than an assign function. This
#' is because the {datashield.assign} function in the data repository deals specially with
#' a created object (newobj) if it is of class list. Reconfiguring the
#' function as an aggregate function works around this problem.
#' This aggregate function is based on the native R function {unlist}
#' and so additional information can be found in the help for {unlist}.
#' When an object is coerced to a list, depending
#' on the class of the original object some information may be lost. Thus,
#' for example, when a data.frame is coerced to a list information that
#' underpins the structure of the data.frame is lost and when it is
#' subject to the function {ds.unlist} it is returned to a simpler
#' class than data.frame eg 'numeric' (basically a numeric vector
#' containing all of the original data in all variables in the data.frame
#' but with no structure). If you wish to reconstruct the original
#' data.frame you therefore need to specify this structure again e.g.
#' the column names etc
#' @param x.name the name of the input object to be coerced back from
#' class list. It must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' {ds.unList}
#' @param recursive argument required for native R unlist function - see
#' native R help for {unlist} function
#' @param newobj is the object hard assigned '<<-' to be the output of the
#' function written to the serverside
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.mat) which is written to the serverside.
#' In addition, two validity messages are returned. The first confirms an output
#' object has been created, the second states its class.
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @export
unListDS <- function(x.name,recursive,newobj){

    recursive<-recursive

    newobj.class <- NULL
    if(is.character(x.name)){
        active.text<-paste0(newobj,"<-unlist(",x.name,",",recursive,")")
        eval(parse(text=active.text), envir = parent.frame())
        
        active.text2<-paste0("class(",newobj,")")
        assign("newobj.class", eval(parse(text=active.text2), envir = parent.frame()))
    }else{
        studysideMessage<-"ERROR: x.name must be specified as a character string"
        return(list(studysideMessage=studysideMessage))
    }

    return.message<-paste0("New object <",newobj,"> created")
    object.class.text<-paste0("Class of <",newobj,"> is '",newobj.class,"'")

    return(list(return.message=return.message,class.of.newobj=object.class.text))
}
#AGGREGATE FUNCTION
# unListDS
