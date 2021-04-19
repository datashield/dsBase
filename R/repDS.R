#' @title repDS called by ds.rep
#' @description An assign function which
#' creates a repetitive sequence by repeating
#' an identified scalar, or specified elements of a vector
#' or list. This is analogous to the {rep} function in native R.
#' The sequence is written as a new object to the serverside
#' @details Further details can be found in the help details
#' for on ds.rep and the following aspects of the help for
#' the function {rep} in native R also apply (as explained
#' in more detail with exceptions identified in help for {ds.rep}):
#'
#' In addition a Details from R help for <rep>:
#' 
#' The default behaviour is as if the call was 
#' rep(x, times = 1, length.out = NA, each = 1)
#' Normally just one of the additional arguments is specified, but if 'each' is
#' specified with either of the other two, its replication is performed first, and
#' then that is followed by the replication implied by times or length.out.
#' 
#' If times consists of a single integer, the result consists of the whole input
#' repeated this many times. If times is a vector of the same length as x (after
#' replication by each), the result consists of x[1] repeated times[1] times, x[2]
#' repeated times[2] times and so on. ***Note exception 1 above.
#' 
#' length.out may be given in place of times, in which case x is repeated as many
#' times as is necessary to create a vector of this length. If both are given,
#' length.out takes priority and times is ignored. ***Note exception 3 above.
#' 
#' Non-integer values of times will be truncated towards zero. If times is a
#' computed quantity it is prudent to add a small fuzz or use round. And analogously
#' for each.

#' @param x1.transmit This argument determines the input scalar, vector or list.
#' for behaviour see help for {ds.rep} and "details from native R
#' help for <rep>" (see above). This parameter is usually fully defined by
#' the argument <x1> in the call to {ds.rep} that itself calls {repDS}.
#' @param times.transmit This argument determines the number of replications
#' and the pattern of these replications of the input scalar/vector to
#' construct the output repetitive sequence.
#' For behaviour see help for {ds.rep} and "details from native R
#' help for <rep>" (see above). This parameter is usually fully defined by
#' the argument <times> in the call to {ds.rep} that itself calls {repDS}.
#' @param length.out.transmit This argument fixes the length of
#' the output repetive sequence vector
#' For behaviour see help for {ds.rep} and "details from native R
#' help for <rep>" (see above). This parameter is usually fully defined by
#' the argument <length.out> in the call to {ds.rep} that itself calls {repDS}.
#' @param each.transmit This argument specifies the number of replications
#' of individual elements rather than replications of the full sequence.
#' For behaviour see help for {ds.rep} and "details from native R
#' help for <rep>" (see above). This parameter is usually fully defined by
#' the argument <each> in the call to {ds.rep} that itself calls {repDS}.
#' @param x1.includes.characters Boolean parameter determining
#' whether to coerce the final output sequence to numeric. Defaults
#' to FALSE and output is coerced to numeric.
#' For detailed behaviour see help for {ds.rep}.
#' This parameter is usually fully defined by
#' the argument <x1.includes.characters> in the call to {ds.rep} that
#' itself calls {repDS}.
#' @param source.x1 This defines the source of the scalar or vector defined
#' by the <x1> argument. Four character strings are allowed:
#' "clientside" or "c" and serverside or "s".
#' For behaviour see help for {ds.rep} and "details from native R
#' help for <rep>" (see above). This parameter is usually fully defined by
#' the argument <source.x1> in the call to {ds.rep} that itself calls {repDS}.
#' @param source.times see "param source.x1"
#' This parameter is usually fully defined by
#' the argument <source.times> in the call to {ds.rep} that itself calls {repDS}.
#' @param source.length.out see "param source.x1"
#' This parameter is usually fully defined by
#' the argument <source.length.out> in the call to {ds.rep} that itself calls {repDS}.
#' @param source.each see "param source.x1"
#' This parameter is usually fully defined by
#' the argument <source.each> in the call to {ds.rep} that itself calls {repDS}.
#' @return the vector containing the specified repetitive sequence
#' and write to the output object defined by the <newobj> argument
#' (or default name seq.vect) which is written to the serverside in 
#' each source. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.matrixDiag also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team, 14/10/2019
#' @export
#'
repDS <- function(x1.transmit, times.transmit, length.out.transmit, each.transmit,
                   x1.includes.characters, source.x1, source.times, source.length.out, source.each){
#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
#thr<-listDisclosureSettingsDS()                                        #
#nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
#nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
#nfilter.string<-as.numeric(thr$nfilter.string)                         #
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)               #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################

#DONT DO LENGTH CHECKS BECAUSE CLIENTSIDE VECTORS MAY REASONABLY BE LONG
#Check length of x1.transmit string not so long as to provide a risk of hidden code
#length.x1.transmit<-length(strsplit(x1.transmit,''))



########################################################
#Evaluate x1.transmit via route depending on its source#
########################################################

#x1 can be either a vector or scalar (technically using the
#fact that a scalar is a special case of a vector). It may
#defined on either the clientside or serverside and we need
#to deal with these two settings differently. x1 cannot
#be NULL but it can be a vector of character strings
#rather than numeric (unlike the other arguments to
#this function). These all influence how the x1 argument
#must be processed both on clientside and serverside and
#what error messages are required.

#############################################
#Check x1.transmit string is safe before eval
#############################################
#set test string for failure under QA to ":;"
#Can't force illegal codes into the name of a serverside object because it gets blocked by the parser
#so can't test the checking procedure for including illegal characters in a serverside variable or scalar name
#if someone bypasses the parser. But this is simpler than the clientside testing so assume works


x1.text<-strsplit(x1.transmit, split=",")
string.2.test<-x1.text[[1]]

string.safe<-TRUE

if(!is.character(string.2.test))string.safe<-FALSE

one.string<-paste(string.2.test,sep=",",collapse="")
chars.separated<-strsplit(one.string, split="")[[1]]
lencs<-length(chars.separated)

	#set test string for failure under QA to ":;"
	if(lencs==1)
	{
		if((chars.separated[1]=="=" || chars.separated[1]=="<"))string.safe<-FALSE
	}
	else
	{
		for(j in 1:(lencs-1))
		{
		if((chars.separated[j]=="=" || chars.separated[j+1]=="=") ||
		(chars.separated[j]=="<" && chars.separated[j+1]=="-") ||
		(chars.separated[j]==":" && chars.separated[j+1]==";"))string.safe<-FALSE
		}
	}



if(!string.safe)
{
   studysideMessage<-"FAILED: the object addressed by the x1.transmit argument is of an inappropriate class or it contains characters that could indicate malicious code. Where possible, please use standard alphanumerics in the elements of the clientside scalar/vector, or the name of the serverside scalar/vector, that is addressed by the x1.transmit argument. As a minimum you MUST avoid '=' and '<' as characters" 
   stop(studysideMessage, call. = FALSE)
}

if(source.x1=="serverside")
{
#x1.transmit is the name of a serverside vector or scalar

x1.use<-eval(parse(text=x1.transmit), envir = parent.frame())
}

if(source.x1=="clientside")
{
#x1 is a transmission modified clientside vector or scalar
x1.temp<-eval(parse(text=list(x1.text)), envir = parent.frame())
x1.c<-x1.temp[[1]]

#unlike other arguments of ds.rep, x1 can be numeric or characters
#if x1 is all numeric coerce to numeric. Otherwise leave as character

	if(x1.includes.characters)
	{
	x1.use<-x1.c
	}else{
	x1.use<-as.numeric(x1.c)
	}

}

if(!is.vector(x1.use))
{
   studysideMessage<-"FAILED: x1 argument must define a clientside or serverside vector or scalar"
   stop(studysideMessage, call. = FALSE)
}


########################################################
#Evaluate times.transmit via route depending on its source#
########################################################

#<times> can define either a vector or scalar. It may
#be defined on either the clientside or serverside. We need
#to deal with all of these settings appropriately.
#The argument <source.times> may be NULL but times
#itself can only be numeric. These issues
#all influence how the <times> argument
#must be processed both on clientside and serverside and
#what error messages are required. 

################################################
#Check times.transmit string is safe before eval
################################################

#Argument can only be numeric so cannot encode identifiable danger code
#However, this failure occurs when primary function is called at bottom of
#code, and it can only be checked to be numeric after eval
#so check no embedded assign codes anyway:
#set test string for failure under QA to ":;"
#Can't force illegal codes into the name of a serverside object because it gets blocked by the parser
#so can't test the checking procedure for including illegal characters in a serverside variable or scalar name
#if someone bypasses the parser. But this is simpler than the clientside testing so assume works


if(!is.null(times.transmit))
{
	times.text<-strsplit(times.transmit, split=",")
	string.2.test<-times.text[[1]]

	string.safe<-TRUE

	if(!is.character(string.2.test))string.safe<-FALSE

	one.string<-paste(string.2.test,sep=",",collapse="")
	chars.separated<-strsplit(one.string, split="")[[1]]
	lencs<-length(chars.separated)

	#set test string for failure under QA to ":;"
	if(lencs==1)
	{
		if((chars.separated[1]=="=" || chars.separated[1]=="<"))string.safe<-FALSE
	}
	else
	{
		for(j in 1:(lencs-1))
		{
		if((chars.separated[j]=="=" || chars.separated[j+1]=="=") ||
		(chars.separated[j]=="<" && chars.separated[j+1]=="-") ||
		(chars.separated[j]==":" && chars.separated[j+1]==";"))string.safe<-FALSE
		}
	}

	if(!string.safe)
	{
	studysideMessage<-"FAILED: the object addressed by the times.transmit argument is of an inappropriate class or it contains characters that could indicate malicious code. Where possible, please use standard alphanumerics in the elements of the clientside scalar/vector, or the name of the serverside scalar/vector, that is addressed by the x1.transmit argument. As a minimum you MUST avoid '=' and '<' as characters" 
	stop(studysideMessage, call. = FALSE)
	}
}

if(is.null(source.times))
{
#no times value is specified
times.use<-1
}
else
{
	if(source.times=="serverside")
	{
	#times.transmit is the name of a serverside vector or scalar

	times.use<-eval(parse(text=times.transmit), envir = parent.frame())
	}

	if(source.times=="clientside")
	{
	#times.transmit is a transmission modified clientside vector or scalar

	times.text<-strsplit(times.transmit, split=",")

	times.c<-eval(parse(text=times.text), envir = parent.frame())

	times.use<-as.numeric(times.c)
	
	}
}

##########################################################
#Evaluate length.out.transmit via route depending on its source#
##########################################################

#The argument <length.out> can either be a
#clientside or serverside scalar or a serverside vector.
#We need to deal with these three settings differently.
#The argument <source.length.out> may be NULL but length.out
#itself can only be numeric. These issues
#all influence how the <length.out> argument
#must be processed both on clientside and serverside and
#what error messages are required.

################################################
#Check length.out.transmit string is safe before eval
################################################

#Argument can only be numeric so cannot encode identifiable danger code
#However, this failure occurs when primary function is called at bottom of
#code, and it can only be checked to be numeric after eval
#so check no embedded assign codes anyway:
#set test string for failure under QA to ":;"
#Can't force illegal codes into the name of a serverside object because it gets blocked by the parser
#so can't test the checking procedure for including illegal characters in a serverside variable or scalar name
#if someone bypasses the parser. But this is simpler than the clientside testing so assume works

if(!is.null(length.out.transmit))
{
	length.out.text<-strsplit(length.out.transmit, split=",")
	string.2.test<-length.out.text[[1]]

	string.safe<-TRUE

	if(!is.character(string.2.test))string.safe<-FALSE

	one.string<-paste(string.2.test,sep=",",collapse="")
	chars.separated<-strsplit(one.string, split="")[[1]]
	lencs<-length(chars.separated)

	#set test string for failure under QA to ":;"
	if(lencs==1)
	{
		if((chars.separated[1]=="=" || chars.separated[1]=="<"))string.safe<-FALSE
	}
	else
	{
		for(j in 1:(lencs-1))
		{
		if((chars.separated[j]=="=" || chars.separated[j+1]=="=") ||
		(chars.separated[j]=="<" && chars.separated[j+1]=="-") ||
		(chars.separated[j]==":" && chars.separated[j+1]==";"))string.safe<-FALSE
		}
	}

	if(!string.safe)
	{
	studysideMessage<-"FAILED: the object addressed by the length.out.transmit argument is of an inappropriate class or it contains characters that could indicate malicious code. Where possible, please use standard alphanumerics in the elements of the clientside scalar/vector, or the name of the serverside scalar/vector, that is addressed by the x1.transmit argument. As a minimum you MUST avoid '=' and '<' as characters" 
	stop(studysideMessage, call. = FALSE)
	}
}


if(is.null(source.length.out))
{
#no length.out value is specified
length.out.use<-NA
}
else
{
	if(source.length.out=="serverside")
	{
	#length.out.transmit is the name of the serverside vector or scalar
	length.out.temp<-eval(parse(text=length.out.transmit), envir = parent.frame())
	
	arg.is.vector<-FALSE
	if(length(length.out.temp)>=2)arg.is.vector<-TRUE

		if(arg.is.vector)
		{
		length.out.use<-length(length.out.temp)
		}else
		{
		length.out.use<-length.out.temp
		}
	}

	if(source.length.out=="clientside")
	{
	#length.out.transmit is a clientside scalar
	length.out.text<-strsplit(length.out.transmit, split=",")
	length.out.c<-eval(parse(text=length.out.text), envir = parent.frame())
	length.out.use<-as.numeric(length.out.c)
	}
}


##########################################################
#Evaluate each.transmit via route depending on its source#
##########################################################

#The argument <each> is strictly a scalar.  It may
#be defined either on the clientside or serverside and
#we need to deal with these two settings differently.
#<each> may be NULL but it can only be numeric. These issues
#all influence how the <each> argument
#must be processed both on clientside and serverside and
#what error messages are required.


################################################
#Check each.transmit string is safe before eval
################################################


#Argument can only be numeric so cannot encode identifiable danger code
#However, this failure occurs when primary function is called at bottom of
#code, and it can only be checked to be numeric after eval
#so check no embedded assign codes anyway:
#set test string for failure under QA to ":;"
#Can't force illegal codes into the name of a serverside object because it gets blocked by the parser
#so can't test the checking procedure for including illegal characters in a serverside variable or scalar name
#if someone bypasses the parser. But this is simpler than the clientside testing so assume works

if(!is.null(each.transmit))
{
	each.text<-strsplit(each.transmit, split=",")
	string.2.test<-each.text[[1]]

	string.safe<-TRUE

	if(!is.character(string.2.test))string.safe<-FALSE

	one.string<-paste(string.2.test,sep=",",collapse="")
	chars.separated<-strsplit(one.string, split="")[[1]]
	lencs<-length(chars.separated)

	#set test string for failure under QA to ":;"
	if(lencs==1)
	{
		if((chars.separated[1]=="=" || chars.separated[1]=="<"))string.safe<-FALSE
	}
	else
	{
		for(j in 1:(lencs-1))
		{
		if((chars.separated[j]=="=" || chars.separated[j+1]=="=") ||
		(chars.separated[j]=="<" && chars.separated[j+1]=="-") ||
		(chars.separated[j]==":" && chars.separated[j+1]==";"))string.safe<-FALSE
		}
	}

	if(!string.safe)
	{
	studysideMessage<-"FAILED: the object addressed by the each.transmit argument is of an inappropriate class or it contains characters that could indicate malicious code. Where possible, please use standard alphanumerics in the elements of the clientside scalar/vector, or the name of the serverside scalar/vector, that is addressed by the x1.transmit argument. As a minimum you MUST avoid '=' and '<' as characters" 
	stop(studysideMessage, call. = FALSE)
	}
}

if(is.null(source.each))
{
#no each value is specified
each.use<-NA
}
else
{
	if(source.each=="serverside")
	{
	#each.transmit is the name of the serverside vector or scalar
	each.use<-eval(parse(text=each.transmit), envir = parent.frame())
	}

	if(source.each=="clientside")
	{
	#each.transmit is a clientside vector or scalar
	each.text<-strsplit(each.transmit, split=",")
	each.c<-eval(parse(text=each.text), envir = parent.frame())
	each.use<-as.numeric(each.c)
	}
}


#CREATE newobj
out.vect<-rep(x=x1.use,times=times.use,length.out=length.out.use,each=each.use)
}

#assign function
# repDS

