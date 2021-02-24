#' @title asListDS a serverside aggregate function called by ds.asList
#' @description Coerces an R object into a list
#' @details Unlike most other class coercing functions this is
#' an aggregate function rather than an assign function. This
#' is because the {datashield.assign} function in the data repository deals specially with
#' a created object (newobj) if it is of class list. Reconfiguring the
#' function as an aggregate function works around this problem.
#' This aggregate function is based on the native R function {as.list}
#' and so additional information can be found in the help for {as.list}
#' @param x.name the name of the input object to be coerced to class
#' data.matrix. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' {ds.asList}
#' @param newobj is the object hard assigned '<<-' to be the output of the
#' function written to the serverside
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.mat) which is written to the serverside.
#' In addition, two validity messages are returned. The first confirms an output
#' object has been created, the second states its class. The way that {as.list}
#' coerces objects to list depends on the class of the object, but in general
#' the class of the output object should usually be 'list'
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @export
asListDS <- function (x.name,newobj){

    newobj.class <- NULL
    if(is.character(x.name)){
        active.text<-paste0(newobj,"<-as.list(",x.name,")")
        eval(parse(text=active.text), envir = parent.frame())

        active.text2<-paste0("class(",newobj,")")
        assign("newobj.class", eval(parse(text=active.text2), envir = parent.frame()))

    }else{
        studysideMessage<-"ERROR: x.name must be specified as a character string"
        stop(studysideMessage, call. = FALSE)
    }

    return.message<-paste0("New object <",newobj,"> created")
    object.class.text<-paste0("Class of <",newobj,"> is '",newobj.class,"'")

    return(list(return.message=return.message,class.of.newobj=object.class.text))
}
#AGGEGATE FUNCTION
# asListDS
