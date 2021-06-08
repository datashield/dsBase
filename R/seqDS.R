#' @title seqDS a serverside assign function called by ds.seq
#' @description assign function seqDS called by ds.seq
#' @details An assign function that uses the native R function seq() to create
#' any one of a flexible range of sequence vectors that can then be used to help
#' manage and analyse data. As it is an assign function the resultant vector is
#' written as a new object into all of the specified data source servers. Please
#' see "details" for {ds.seq} for more information about allowable combinations
#' of arguments etc. 
#' @param FROM.value.char the starting value for the sequence expressed as an integer
#' or real number with a decimal point but 
#' in character form. Fully specified by <FROM.value.char> argument
#' of {ds.seq}.
#' @param TO.value.char the terminal value for the sequence expressed as an integer
#' or real number with a decimal point but 
#' in character form. Fully specified by <TO.value.char> argument
#' of {ds.seq}.
#' @param BY.value.char the value to increment each step in the sequence
#' expressed as an integer
#' or real number with a decimal point but 
#' in character form. Fully specified by <BY.value.char> argument
#' of {ds.seq}.
#' @param LENGTH.OUT.value.char length of the sequence at which point
#' its extension should be stopped, expressed as an integer
#' or real number with a decimal point but 
#' in character form. Fully specified by <LENGTH.OUT.value.char> argument
#' of {ds.seq}.
#' @param ALONG.WITH.name For convenience, rather than specifying a value
#' for LENGTH.OUT it can often be better to specify a variable name as
#' the <ALONG.WITH.name> argument. Fully specified by <ALONG.WITH.name> argument
#' of {ds.seq}.
#' @return the object specified by the <newobj> argument of
#' {ds.seq} (or its default name newObj)
#' which is written to the serverside. 
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
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("<newobj>")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team, 17/9/2019
#' @export
seqDS <- function(FROM.value.char,TO.value.char,BY.value.char,LENGTH.OUT.value.char,ALONG.WITH.name)
{

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
#thr<-.AGGREGATE$listDisclosureSettingsDS()                             #
thr<-listDisclosureSettingsDS()                                         #
nfilter.tab<-as.numeric(thr$nfilter.tab)                                #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
nfilter.subset<-as.numeric(thr$nfilter.subset)                          #
#nfilter.string<-as.numeric(thr$nfilter.string)                         #
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)               #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################


 if(is.character(FROM.value.char)&&is.numeric(eval(parse(text=FROM.value.char), envir = parent.frame()))){
	FROM<-eval(parse(text=FROM.value.char), envir = parent.frame())
	}else{
   studysideMessage<-"ERROR: FROM.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
   stop(studysideMessage, call. = FALSE)
   }
 
 if(!is.null(TO.value.char))
	{
	if(is.character(TO.value.char)&&is.numeric(eval(parse(text=TO.value.char), envir = parent.frame())))
		{
		TO<-eval(parse(text=TO.value.char), envir = parent.frame())
		}else{
		studysideMessage<-"ERROR: TO.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
		stop(studysideMessage, call. = FALSE)
		}
	}
 
 if(is.null(TO.value.char))
 {
 TO<-NULL
 }
	
 if(is.character(BY.value.char)&&is.numeric(eval(parse(text=BY.value.char), envir = parent.frame()))){
	BY<-eval(parse(text=BY.value.char), envir = parent.frame())
	}else{
   studysideMessage<-"ERROR: BY.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
   stop(studysideMessage, call. = FALSE)
   }

 if(!is.null(LENGTH.OUT.value.char)){
		if(is.character(LENGTH.OUT.value.char)&&is.numeric(eval(parse(text=LENGTH.OUT.value.char), envir = parent.frame()))){
		LENGTH.OUT<-eval(parse(text=LENGTH.OUT.value.char), envir = parent.frame())
		}else{
		studysideMessage<-"ERROR: If LENGTH.OUT.value.char is non-NULL, it must specify a positive integer in inverted commas eg '14'" 
		stop(studysideMessage, call. = FALSE)
		}
	}

 if(is.null(LENGTH.OUT.value.char)){
 LENGTH.OUT<-NULL
 }
 
 if(!is.null(ALONG.WITH.name)){
		if(is.character(ALONG.WITH.name)){
		ALONG.WITH<-eval(parse(text=ALONG.WITH.name), envir = parent.frame())
		}else{
		studysideMessage<-"ERROR: If ALONG.WITH.name is non-NULL, it must specify the name of a serverside vector in inverted commas" 
		stop(studysideMessage, call. = FALSE)
		}
	}
 
 if(is.null(ALONG.WITH.name)){
 ALONG.WITH<-NULL
 }

 
if(!is.null(TO)){ 
output.vector<-seq(from=FROM,to=TO,by=BY)
}

if(!is.null(LENGTH.OUT)){ 
output.vector<-seq(from=FROM,by=BY,length.out=LENGTH.OUT)
}

if(!is.null(ALONG.WITH)){ 
output.vector<-seq(from=FROM,by=BY,along.with=ALONG.WITH)
}

return(output.vector)

}
#ASSIGN FUNCTION
# seqDS

