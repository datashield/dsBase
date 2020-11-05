#' @title unListDS a serverside assign function called by ds.unList
#' @description this function is based on the native R function {unlist}
#' which coerces an object of list class back to the class it was when
#' it was coerced into a list
#' @details See details of the native R function {unlist}.
#' This function represents a substantive restructuring of an earlier version
#' created by Amadou Gaye. For further details of its working please
#' see 'details' in the help for {ds.unList}.
#' @param x.name the name of the input object to be unlisted.
#' It must be specified in inverted commas e.g. x.name="input.object.name". Fully
#' specified by the <x.name> argument of {ds.unList}
#' @return the object specified by the <newobj> argument of the 
#' {ds.unList} function (or by default <x.name>.unlist
#' if the <newobj> argument is NULL). This is written to the serverside.
#' As well as writing the output object as <newobj>
#' on the serverside, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' {ds.seq()} also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("<newobj>") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. Because the outcome object from ds.unList is
#' typically a list object with no names, if there are no errors in creating it
#' the message returned from ds.message("<newobj>") in each study will read
#' "Outcome object is a list without names. So a studysideMessage may be hidden.
#' Please check output is OK". This suggests that - in the case of this specific
#' function - one should check as far as one can
#' the nature of the output from a call to ds.unList - e.g. ds.class, ds.length etc
#' @author Amadou Gaye (2016), Paul Burton (19/09/2019) for DataSHIELD Development Team
#' @export
unListDS <- function(x.name) {

    if (is.character(x.name)) {
	listvar<-eval(parse(text=x.name), envir = parent.frame())
    } else {
        studysideMessage<-"ERROR: x.name must be specified as a character string"
        return(list(studysideMessage=studysideMessage))
    } 
 
    outvar<-unlist(listvar)

    return(outvar)
}
#ASSIGN FUNCTION
# unListDS

