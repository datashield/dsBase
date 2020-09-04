#'
#' @title messageDS
#' @description This function allows for error messages arising from the
#' running of a server-side assign function to be returned to the client-side
#' @details Errors arising from aggregate server-side functions can be returned
#' directly to the client-side. But this is not possible for server-side assign
#' functions because they are designed specifically to write objects to the
#' server-side and to return no meaningful information to the client-side.
#' Otherwise, users may be able to use assign functions to return disclosive
#' output to the client-side. ds.message calls messageDS which looks
#' specifically for an object called $serversideMessage in a designated list on
#' the server-side. Server-side functions from which error messages are to be made
#' available, are designed to be able to write the designated error message to
#' the $serversideMessage object into the list that is saved on the server-side
#' as the primary output of that function. So only valid server-side functions of
#' DataSHIELD can write a $studysideMessage, and as additional protection against
#' unexpected ways that someone may try to get round this limitation, a
#' $studysideMessage is a string that cannot exceed a length of nfilter.string
#' a default of 80 characters.
#' @param message.object.name is a character string, containing the name of the list containing the
#' message. See the header of the client-side function ds.message for more details.
#' @return a list object from each study, containing whatever message has been written by
#' DataSHIELD into $studysideMessage.
#' @author Burton PR
#' @export
#'
messageDS <- function(message.object.name){

#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
thr<-listDisclosureSettingsDS()                             #
#nfilter.tab<-as.numeric(thr$nfilter.tab)                   #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                   #
nfilter.subset<-as.numeric(thr$nfilter.subset)              #
nfilter.string<-as.numeric(thr$nfilter.string)              #
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    #
nfilter.kNN<-as.numeric(thr$nfilter.kNN)                    #
#############################################################

#TEST IF message.object.name EXISTS AS AN OBJECT ON SERVERSIDE
#exists() must have message.object.name explicitly specified in inverted commas.
#NOT just the name of a character class object representing the name without
#inverted comma

A1.exists.text<-paste0("exists('",message.object.name,"')")

boole.A1.exists<-eval(parse(text=A1.exists.text), envir = parent.frame())




if(!boole.A1.exists) {
out.obj<-"Error: the object <message.object.name> does not exist in this datasource"
return(out.obj)
}



#IF message.object.name EXISTS, CHECK WHETHER IT CURRENTLY CONTAINS A studysideMessage

if(boole.A1.exists){
message.object.name.active<-eval(parse(text=message.object.name), envir = parent.frame())



#CASE WHERE PRIMARY OUTPUT OBJECT IS A LIST
if("list" %in% class(message.object.name.active)){

#A LIST WITH NAMES - could be a studysideMessage
	if(!is.null(names(message.object.name.active)))	{
		s.message.available<-FALSE
		for(j in 1:length(names(message.object.name.active))){
			if(names(message.object.name.active)[j]=="studysideMessage"){
			s.message.available<-TRUE
			out.obj<-"NOT ALL OK: there are studysideMessage(s) on this datasource"
			}
		}
		if(!s.message.available){
			out.obj<-"ALL OK: there are no studysideMessage(s) on this datasource"
		}
	}

#A LIST WITHOUT NAMES
	if(is.null(names(message.object.name.active)))	{
	out.obj<-"Outcome object is a list without names. So a studysideMessage may be hidden. Please check output is OK"
	}

}else{
#CASE WHERE PRIMARY OUTPUT IS AN OBJECT OF A CLASS OTHER THAN LIST - output object created OK and so no studysideMessage
	out.obj<-"ALL OK: there are no studysideMessage(s) on this datasource"
	}

  }

   return(MESSAGE=out.obj)

}
#AGGREGATE FUNCTION
# messageDS
