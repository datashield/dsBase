###############################################################################|###################| 80 and 100#'
#' @title recodeValuesDS2 an assign function called by ds.recodeValues
#' @description Second serverside function called by ds.recodeValues
#' to convert specified values
#' of elements in a vector into a matched set of alternative specified values.
#' @details For all details see the help header for ds.recodeValues
#' @param var.name.text a character string providing the name for the vector representing the
#' variable to be recoded. <var.name.text> argument generated and passed directly to
#' recodeValuesDS2 by ds.recodeValues
#' @param values2replace.text a character string specifying the values in the
#' vector specified by the argument <var.name.text> that are to be replaced by new
#' values as specified in the new.values.vector. The <values2replace.text> argument
#' is generated and passed directly to recodeValuesDS2 by ds.recodeValues. In effect, the
#' <values2replace.vector> argument of the ds.recodeValues function is converted
#' to a character string format that is acceptable to the DataSHIELD parser in Opal
#' and so can be accepted by recodeValuesDS2
#' @param new.values.text a character string specifying the new values to which
#' the specified values in the vector <var.name> are to be converted.
#' The <new.values.text> argument is generated and passed directly to recodeValuesDS2
#' by ds.recodeValues. In effect, the <new.values.vector> argument of the
#' ds.recodeValues function is converted to a character string format that is
#' acceptable to the DataSHIELD parser in Opal
#' and so can be used in the call to recodeValuesDS2
#' @param numeric.output.format.possible logical, if TRUE the nature of
#' <var.name>, <values2replace.vector> and <new.values.vector> are such
#' that it is in principle possible for the output to be fully numeric.
#' This argument is  generated and passed directly to recodeValuesDS2
#' by ds.recodeValues - its value determines how recodeValuesDS2
#' handles situations where a numeric output may be desirable.
#' @param force.output.format character string. This argument is generated
#' and passed directly to recodeValuesDS2 by ds.recodeValues. For details
#' see the equivalent parameter in the help header for ds.recodeValues
#' @param v2r.numeric logical. This argument is generated
#' and passed directly to recodeValuesDS2 by ds.recodeValues. If TRUE
#' it informs recodeValuesDS2 that the nature of
#' <var.name>, <values2replace.vector>, <new.values.vector> and <force.output.format>
#' are such that recodeValuesDS2 should convert the recoded (output) vector to numeric.
#' If false, recodeValuesDS2 should write out the recoded (output) vector as character.
#' @return the object specified by the <newobj> argument (or default name '<var.name>_recoded')
#' initially specified in calling ds.recodeValues. The output object (the required
#' recoded variable called <newobj> is written to the serverside. In addition,
#' two validity messages are returned via ds.recodeValues
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' recodeValuesDS2 (via ds.recodeValues()) also returns any studysideMessages
#' that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export
#'
recodeValuesDS2 <- function(var.name.text=NULL, values2replace.text=NULL, new.values.text=NULL,numeric.output.format.possible,force.output.format="no",v2r.numeric=NULL){

#############################################################
#MODULE 1: CAPTURE THE nfilter SETTINGS                     #
thr <- listDisclosureSettingsDS()				#
#nfilter.tab<-as.numeric(thr$nfilter.tab)					#
#nfilter.glm<-as.numeric(thr$nfilter.glm)					#
nfilter.subset<-as.numeric(thr$nfilter.subset)          	#
nfilter.string<-as.numeric(thr$nfilter.string)              #
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    #
nfilter.kNN<-as.numeric(thr$nfilter.kNN)                    #
#############################################################


#DISCLOSURE TRAPS
var.name.text.chars<-strsplit(var.name.text,split="")
if(length(var.name.text.chars[[1]])>nfilter.stringShort){
   studysideMessage<-"Error: var.name.text argument too long (see nfilter.stringShort)"
   return(list(studysideMessage=studysideMessage))
  }

values2replace.text.chars<-strsplit(values2replace.text,split="")
if(length(values2replace.text.chars[[1]])>nfilter.stringShort){
   studysideMessage<-"Error: values2replace.text argument too long (see nfilter.stringShort)"
   return(list(studysideMessage=studysideMessage))
  }

  new.values.text.chars<-strsplit(new.values.text,split="")
  if(length(new.values.text.chars[[1]])>nfilter.stringShort){
   studysideMessage<-"Error: new.values.text argument too long(see nfilter.stringShort)"
   return(list(studysideMessage=studysideMessage))
  }


var.name.text.c<-unlist(strsplit(var.name.text, split=","))
var2recode<-eval(parse(text=var.name.text.c))


values2replace.c<-unlist(strsplit(values2replace.text, split=","))

if(v2r.numeric){
values2replace<-as.numeric(values2replace.c)
}else{
values2replace<-values2replace.c
}


new.values.c<-unlist(strsplit(new.values.text, split=","))

#SHOULD OUTPUT FORMAT BE NUMERIC OR CHARACTER

new.values<-new.values.c

numeric.output.format.still.possible<-(is.numeric(var2recode)&&numeric.output.format.possible)

if(numeric.output.format.still.possible || force.output.format=="numeric"){
new.values<-as.numeric(new.values.c)
}

if(force.output.format=="character"){
new.values<-new.values.c
}


var.recoded<-var2recode

for(j in 1:length(var2recode)){
	for(k in 1:length(values2replace)){
		if(is.na(var2recode[j])||is.na(values2replace[k])){
			if(is.na(var2recode[j])&&is.na(values2replace[k])){
			var.recoded[j]<-new.values[k]
			}
		}
		else
		{
			if(var2recode[j]==values2replace[k]){
			var.recoded[j]<-new.values[k]
			}
		}
	}
}

return.obj<-var.recoded

#DISCLOSURE TRAP ON LENGTH OF NA AND non-NA ELEMENTS OF ORIGINAL AND RECODED VECTORS
mark.original<-stats::complete.cases(var2recode)
non.NA.original.vector<-var2recode[mark.original]
non.NA.length.original<-length(non.NA.original.vector)

mark.recoded<-stats::complete.cases(var.recoded)
non.NA.recoded.vector<-var.recoded[mark.recoded]
non.NA.length.recoded<-length(non.NA.recoded.vector)

difference.non.NA.lengths<-abs(non.NA.length.recoded-non.NA.length.original)


#Non-NA SUBSET OF RECODED VARIABLE SMALLER THAN MINIMUM SUBSET SIZE - BLOCK CREATION OF RECODED VECTOR
#AND RETURN MESSAGE
if(non.NA.length.recoded<nfilter.subset){
   studysideMessage<-"Error: number of non-NA elements of recoded vector < minimum subset size"
   return(list(studysideMessage=studysideMessage))
  }


return(return.obj)

 }
#ASSIGN FUNCTION
# recodeValuesDS2
