#'
#' @title seqDS.o called by ds.seq.o
#' @description assign function seqDS.o called by ds.seq.o
#' @details An assign function that uses the native R function seq() to create
#' any one of a flexible range of sequence vectors that can then be used to help
#' manage and analyse data. As it is an assign function the resultant vector is
#' written as a new object onto all of the specified data source servers. For
#' the purposes of creating the DataSHIELD equivalent to seq() in native R we
#' have used all of the original arguments (see below) except the <to> argument.
#' This simplifies the function and prevents some combinations of arguments that
#' lead to an error in native R. The effect of the <to> argument - see help(seq) in
#' native R - is to specify the terminal value of the sequence. However,
#' when using seq() one can usually specify other arguments (see below) to mimic
#' the desire effect of <to>. These include: <from>, the starting value of the
#' sequence; <by>, its increment (+ or -), and <length.out> the length of the final vector
#' in each data source. 
#' @param FROM.value.char the starting value for the sequence expressed as an integer
#' in character form. e.g. FROM.value.char="1" will start at 1, FROM.value.char="-10"
#' will start at -10. Default = "1". The value of this argument is usually
#' specified by the value provided to the argument <FROM.value.char>
#' in the function {ds.seq.o}
#' @param BY.value.char the value to increment each step in the sequence
#' expressed as a numeric e.g. BY.value.char="10" will increment by 10,
#' while BY.value.char="-3.37" will reduce the value of each sequence
#' element by -3.37. Default = "1" but does not have to be integer.
#' The value of this argument is usually specified by the value provided
#' to the argument <BY.value.char> in the function {ds.seq.o}
#' @param LENGTH.OUT.value.char The length of the sequence at which point
#' its extension should be stopped. e.g.  LENGTH.OUT.value.char="1000" will
#' generate a sequence of length 1000. Default = NULL (must be specified) but
#' must be a positive integer. The value of this argument is usually specified
#' by the value provided to the argument <LENGTH.OUT.value.char>
#' in the function {ds.seq.o}
#' @param ALONG.WITH.name For convenience, rather than specifying a value
#' for LENGTH.OUT it can often be better to specify a variable name as
#' the <ALONG.WITH.name> argument. e.g. ALONG.WITH.name = "vector.name".
#' This can be particularly useful in DataSHIELD
#' where the length of the sequence you need to generate in each data set
#' depends on the standard length of vectors in that data set and this will
#' in general vary. The value of this argument is usually specified
#' by the value provided to the argument <ALONG.WITH.name>
#' in the function {ds.seq.o}
#' @return the object specified by the <newobj> argument of function
#' ds.seq.o (or default name newObj) which is written to the serverside. 
#' As well as writing the output object as <newobj>
#' on the serverside, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' {ds.seq.o()} also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("<newobj>") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("<newobj>")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team
#' @export
seqDS.o <- function(FROM.value.char,BY.value.char,LENGTH.OUT.value.char,ALONG.WITH.name)
{

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS.o()							#
nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################


 
 if(is.character(FROM.value.char)&&is.numeric(eval(parse(text=FROM.value.char)))){
	FROM<-eval(parse(text=FROM.value.char))
	}else{
   studysideMessage<-"ERROR: FROM.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
   return(list(studysideMessage=studysideMessage))
   }

 if(is.character(BY.value.char)&&is.numeric(eval(parse(text=BY.value.char)))){
	BY<-eval(parse(text=BY.value.char))
	}else{
   studysideMessage<-"ERROR: BY.value.char must be specified as a real number in inverted commas eg '-3.74' or '0'"
   return(list(studysideMessage=studysideMessage))
   }

 
 if(!is.null(LENGTH.OUT.value.char)){
		if(is.character(LENGTH.OUT.value.char)&&is.numeric(eval(parse(text=LENGTH.OUT.value.char)))){
		LENGTH.OUT<-eval(parse(text=LENGTH.OUT.value.char))
		}else{
		studysideMessage<-"ERROR: If LENGTH.OUT.value.char is non-NULL, it must specify a positive integer in inverted commas eg '14'" 
		return(list(studysideMessage=studysideMessage))
		}
	}

 if(is.null(LENGTH.OUT.value.char)){
 LENGTH.OUT<-NULL
 }

 if(!is.null(ALONG.WITH.name)){
		if(is.character(ALONG.WITH.name)){
		ALONG.WITH<-eval(parse(text=ALONG.WITH.name))
		}else{
		studysideMessage<-"ERROR: If ALONG.WITH.name is non-NULL, it must specify the name of a serverside vector in inverted commas" 
		return(list(studysideMessage=studysideMessage))
		}
	}
 
 if(is.null(ALONG.WITH.name)){
 ALONG.WITH<-NULL
 }
 
#the <to> argument to seq must be missed out altogether. The output length will then by default
#be determined by the length of the vector specified by the <ALONG.WITH> argument even if
#a value for the <LENGTH.OUT> argument is set. If you want to specify the output length
#with the <LENGTH.OUT> argument you must miss out the <ALONG.WITH> argument altogether

if(!is.null(ALONG.WITH)){ 
output.vector<-seq(from=FROM,by=BY,length.out=LENGTH.OUT,along.with=ALONG.WITH)
}else{
output.vector<-seq(from=FROM,by=BY,length.out=LENGTH.OUT)
}
 
return(output.vector)
}
#ASSIGN FUNCTION
# seqDS.o 

