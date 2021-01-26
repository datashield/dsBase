#' @title completeCasesDS: an assign function called by ds.completeCases
#' @description Identifies and strips out all rows of a data.frame,
#' matrix or vector that contain NAs.
#' @details In the case of a data.frame or matrix, {completeCasesDS} identifies
#' all rows containing one or more NAs and deletes those
#' rows altogether. Any one variable with NA in a given row will lead
#' to deletion of the whole row. In the case of a vector, {completeCasesDS}
#' acts in an equivalent manner but there is no equivalent to a 'row'
#' and so it simply strips out all observations recorded as NA.
#' {ds.completeCASES} is analogous to the {complete.cases} function
#' in native R. Limited additional information can therefore be found
#' under help("complete.cases") in native R.
#' @param x1.transmit This argument determines the input data.frame,
#' matrix or vector from which rows with NAs are to be stripped. 
#' The <x1.transmit> argument is fully specified by the <x1> argument
#' of the {ds.completeCases} function.
#' @return a modified data.frame, matrix or vector from which
#' all rows containing at least one NA have been deleted. This
#' modified object is written to the serverside in each source.
#' In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.completeCases also returns any studysideMessages that can help
#' explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#' 
completeCasesDS <- function(x1.transmit){
  
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
  
  x1.text <- strsplit(x1.transmit, split=",")
  string.2.test <- x1.text[[1]]

  #Check string for eval is safe before activating eval (in case parser breached)
  #eval here provides direct eval of string submitted from client

  string.safe <- TRUE

  if(!is.character(string.2.test)){
    string.safe <- FALSE
  }
  
  one.string <- paste(string.2.test, sep=",", collapse="")
  chars.separated <- strsplit(one.string, split="")[[1]]
  lencs <- length(chars.separated)

	#set test string for failure under QA to ":;"
	if(lencs==1){
		if((chars.separated[1]=="=" || chars.separated[1]=="<")){
		  string.safe <- FALSE
		}  
	}else{
		for(j in 1:(lencs-1)){
		  if((chars.separated[j]=="=" || chars.separated[j+1]=="=") ||
		     (chars.separated[j]=="<" && chars.separated[j+1]=="-") ||
		     (chars.separated[j]==":" && chars.separated[j+1]==";")){
		     string.safe <- FALSE
		  }  
		}
	}

  if(!string.safe){
    studysideMessage <- "FAILED: the object addressed by the x1.transmit argument is of an inappropriate class or it contains characters that could indicate malicious code. Where possible, please use standard alphanumerics in the elements of the clientside scalar/vector, or the name of the serverside scalar/vector, that is addressed by the x1.transmit argument. As a minimum you MUST avoid '=' and '<' as characters" 
    stop(studysideMessage, call. = FALSE)
  }

  #Activate target object
  #x1.transmit is the name of a serverside data.frame, matrix or vector
  x1.use <- eval(parse(text=x1.transmit), envir = parent.frame())
  complete.rows <- stats::complete.cases(x1.use)
  
  if(is.matrix(x1.use) || is.data.frame(x1.use)){
    output.object <- x1.use[complete.rows,]
  }else if(is.atomic(x1.use) || is.factor(x1.use)){
    output.object <- x1.use[complete.rows]
  }else{
	  studysideMessage <- "FAILED: is x1 of wrong class. x1 argument must be a character string defining a serverside matrix, data.frame or vector"
	  stop(studysideMessage, call. = FALSE)
  }
  
  return(output.object)

}	
# assign function
# completeCasesDS
