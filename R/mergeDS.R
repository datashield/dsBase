#' @title mergeDS.o (assign function) called by ds.merge.o
#' @description merges (links) two data.frames together based on common
#' values in defined vectors in each data.frame
#' @details For further information see details of the native R function {merge}
#' and the DataSHIELD clientside function {ds.merge.o}.
#' @param x.name, the name of the first data.frame to be merged specified in
#' inverted commas. Specified via argument <x.name> of {ds.merge.o} function
#' @param y.name, the name of the second data.frame to be merged specified in
#' inverted commas. Specified via argument <y.name> of {ds.merge.o} function
#' @param by.x.names.transmit the name of a single variable or a vector of names of
#' multiple variables (in transmittable form) containing the IDs or other data
#' on which data.frame x is to be merged/linked
#' to data.frame y. Specified via argument <by.x.names> of {ds.merge.o} function
#' @param by.y.names.transmit the name of a single variable or a vector of names of
#' multiple variables (in transmittable form) containing the IDs or other data
#' on which data.frame y is to be merged/linked
#' to data.frame x. Specified via argument <by.y.names> of {ds.merge.o} function
#' @param all.x	logical, if TRUE, then extra rows will be added to the output,
#' one for each row in x that has no matching row in y. Specified via argument
#' <all.x> of {ds.merge.o} function. Default = FALSE.
#' @param all.y	logical, if TRUE, then extra rows will be added to the output,
#' one for each row in y that has no matching row in x. Specified via argument
#' <all.y> of {ds.merge.o} function. Default = FALSE.
#' @param sort logical, if TRUE the merged result should be sorted on elements
#' in the by.x.names and by.y.names columns. Specified via
#' argument <sort> of {ds.merge.o} function. Default = TRUE.
#' @param suffixes.transmit a character vector of length 2 (in transmittable form)
#' specifying the suffixes to be used for making unique common column names
#' in the two input data.frames when they both appear in the merged data.frame.
#' Specified via argument <suffixes> of {ds.merge.o} function. Default '.x' and '.y'.
#' @param no.dups logical, when TRUE suffixes are appended in more cases to
#' rigorously avoid duplicated column names in the merged data.frame.
#' Specified via argument <no.dups> of {ds.merge.o} function. Default TRUE
#' but was apparently implicitly FALSE before R version 3.5.0.
#' @param incomparables, values intended for merging on one column
#' which cannot be matched. See 'match' in help
#' for Native R {merge} function. Specified via argument <incomparables> of
#' {ds.merge.o}
#' @return the merged data.frame specified by the <newobj> argument
#' of ds.merge.o (or by default 'x.name_y.name' if the <newobj> argument
#' is NULL) which is written to the serverside. In addition,
#' two validity messages are returned to the clientside
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study there may
#' be a studysideMessage that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o(<newobj>) it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o(<newobj>)
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Amadou Gaye, Paul Burton, for DataSHIELD Development Team
#' @export
mergeDS.o<-function(x.name, y.name, by.x.names.transmit, by.y.names.transmit, all.x, all.y,
			 sort, suffixes.transmit, no.dups, incomparables)			 			 
{
#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS.o()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)        #
#########################################################################

#manage x.name and y.name
	#check text to be activated is not too long because of disclosure risk
x.name.numchars<-length(unlist(strsplit(x.name, split="")))

if(x.name.numchars>nfilter.stringShort){
   studysideMessage<-
   paste0("Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(studysideMessage=studysideMessage))
}

y.name.numchars<-length(unlist(strsplit(y.name, split="")))
if(y.name.numchars>nfilter.stringShort){
   studysideMessage<-
   paste0("Disclosure risk, number of characters in y.name must not exceed nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(studysideMessage=studysideMessage))
}
	#activate data frame names
x.data.frame<-eval(parse(text=x.name))
y.data.frame<-eval(parse(text=y.name))

#check data.frames are valid data.frames

if(!is.data.frame(x.data.frame)){
   studysideMessage<-"Error: x.name must specify a data.frame"
    return(list(studysideMessage=studysideMessage))
}

if(!is.data.frame(y.data.frame)){
   studysideMessage<-"Error: y.name must specify a data.frame"
    return(list(studysideMessage=studysideMessage))
}


#manage by.x.names and by.y.names
	#check text to be activated is not too long because of disclosure risk

	colnames.x.valid<-TRUE
	num.cols.x<-length(by.x.names.transmit)
	colnames.x<-unlist(strsplit(by.x.names.transmit, split=","))
	for(j in 1:num.cols.x){
		colnames.x.numchars<-length(unlist(strsplit(colnames.x[j], split="")))
		if(colnames.x.numchars>nfilter.stringShort){
		colnames.x.valid<-FALSE
		}
	}
	if(!colnames.x.valid){
    studysideMessage<-
    paste0("Disclosure risk, the number of characters in at least one by.x.name exceeds nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(studysideMessage=studysideMessage))
	}

	colnames.y.valid<-TRUE
	num.cols.y<-length(by.y.names.transmit)
	colnames.y<-unlist(strsplit(by.y.names.transmit, split=","))
	for(j in 1:num.cols.y){
		colnames.y.numchars<-length(unlist(strsplit(colnames.y[j], split="")))
		if(colnames.y.numchars>nfilter.stringShort){
		colnames.y.valid<-FALSE
		}
	}
	if(!colnames.y.valid){
    studysideMessage<-
    paste0("Disclosure risk, the number of characters in at least one by.y.name exceeds nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(studysideMessage=studysideMessage))
	}

	#convert colname format from transmittable to actionable form (a vector of character strings)
	by.x.colnames<-unlist(strsplit(by.x.names.transmit, split=","))
	by.y.colnames<-unlist(strsplit(by.y.names.transmit, split=","))

	
#manage suffixes
	#check text to be activated is not too long because of disclosure risk

	suffixes.valid<-TRUE
	num.suffixes<-length(suffixes.transmit)
	suffixes.to.use<-unlist(strsplit(suffixes.transmit, split=","))
	for(j in 1:num.suffixes){
		suffix.numchars<-length(unlist(strsplit(suffixes.to.use[j], split="")))
		if(suffix.numchars>nfilter.stringShort){
		suffixes.valid<-FALSE
		}
	}

	if(!suffixes.valid){
    studysideMessage<-
    paste0("Disclosure risk, the number of characters in at least one specified suffix exceeds nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(studysideMessage=studysideMessage))
	}

	#convert suffixes format from transmittable to actionable form (a vector of character strings)
	suffixes.character.vector<-unlist(strsplit(suffixes.transmit, split=","))

	output <- merge(x=x.data.frame, y=y.data.frame, by=NULL,
      by.x = by.x.colnames, by.y = by.x.colnames, all=FALSE, all.x = all.x, all.y = all.y,
      sort = sort, suffixes = suffixes.character.vector, no.dups = no.dups,
      incomparables = incomparables)
 
				 
  return(output)
  
  
}
#Assign function
# mergeDS.o
