
###############################################################################|###################| 80 and 100#' @title recodeValuesDS2 an assign function called by ds.recodeValues
#' @title recodeValuesDS1 an aggregate function called by ds.recodeValues
#' @description First serverside function called by ds.recodeValues
#' to convert specified values of elements in a vector into a matched set
#' of alternative specified values.
#' @details For all details see the help header for ds.recodeValues
#' @param var.name.text a character string providing the name for the vector representing the
#' variable to be recoded. The <var.name.text> argument is generated and passed directly to
#' recodeValuesDS2 by ds.recodeValues
#' @param values2replace.text a character string specifying the values in the
#' vector specified by the argument <var.name.text> that are to be replaced by new
#' values as specified in the new.values.vector. The <values2replace.text> argument
#' is generated and passed directly to recodeValuesDS2 by ds.recodeValues. In effect, the
#' <values2replace.vector> argument of the ds.recodeValues function is converted
#' to a character string format that is acceptable to the DataSHIELD R parser in the data repository
#' and so can be accepted by recodeValuesDS1
#' @param new.values.text a character string specifying the new values to which
#' the specified values in the vector identified by the <var.name> argument
#' are to be converted.
#' The <new.values.text> argument is generated and passed directly to recodeValuesDS2
#' by ds.recodeValues. In effect, the <new.values.vector> argument of the
#' ds.recodeValues function is converted to a character string format that is
#' acceptable to the DataSHIELD R parser in the data repository
#' and so can be used in the call to recodeValuesDS1
#' @return This first serverside function called by ds.recodeValues provides
#' first level traps for a comprehensive series of disclosure risks which can be
#' returned directly to the clientside because recodeValuesDS1 is an aggregate
#' function. The second serverside function called by ds.dataFrameSubset
#' (recodeValuesDS2) carries out some of the same disclosure tests, but it is
#' an assign function because it writes the recoded vector to the serverside.
#' In consequence, it records error messages as studysideMessages which can only be
#' retrieved using ds.message
#' @author DataSHIELD Development Team
#' @export
#'
recodeValuesDS1 <- function(var.name.text=NULL, values2replace.text=NULL, new.values.text=NULL){

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


var.name.text.c<-unlist(strsplit(var.name.text, split=","))
var2recode<-eval(parse(text=var.name.text.c), envir = parent.frame())


values2replace.c<-unlist(strsplit(values2replace.text, split=","))
values2replace<-as.numeric(values2replace.c)

new.values.c<-unlist(strsplit(new.values.text, split=","))
new.values<-as.numeric(new.values.c)


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

return.obj<-list(var.recoded=var.recoded)

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
   stop(studysideMessage, call. = FALSE)
  }


########################################################################
##########MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
########################################################################

if((difference.non.NA.lengths<nfilter.subset)&&(difference.non.NA.lengths>0)){
   studysideWarning1<-"Warning: DataSHIELD monitors every session for potentially disclosive analytic requests."
   studysideWarning2<-"The analysis you just submitted has generated a recoded variable in which the number of non-missing"
   studysideWarning3<-"elements differs - but only very slightly - from the original variable. This is most likely to be"
   studysideWarning4<-"an innocent consequence of your recoding needs. However, it could in theory be one step"
   studysideWarning5<-"in a difference-based attack aimed at identifying individuals. This analytic request has"
   studysideWarning6<-"therefore been highlighted in the session log file. Please be reassured, if you do not try"
   studysideWarning7<-"to identify individuals this will cause you no difficulty. However, if you do plan a "
   studysideWarning8<-"malicious attempt to identify individuals by differencing, this will become obvious in the"
   studysideWarning9<-"session log and you will be sanctioned. Possible consequences include loss of future access"
   studysideWarning10<-"to DataSHIELD and/or legal penalties."

   return.message<-list(studysideWarning1,studysideWarning2,studysideWarning3,studysideWarning4,
                   studysideWarning5,studysideWarning6,studysideWarning7,studysideWarning8,
				   studysideWarning9,studysideWarning10)
    }else{
   return.message<-"Recoding undertaken without problems"
	}

########################################################################
##########MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
########################################################################

return(return.message)

}
# AGGREGATE FUNCTION
# recodeValuesDS1
