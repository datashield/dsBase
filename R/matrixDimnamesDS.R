#' @title matrixDimnamesDS assign function called by ds.matrixDimnames
#' @description Adds dimnames (row names, column names or both) to
#' a matrix on the serverside.
#' @details Adds dimnames (row names, column names or both) to
#' a matrix on the serverside. Similar to the {dimnames} function
#' in native R. For more details see help for
#' function {ds.matrixDimnames}
#' @param M1.name Specifies the name of the serverside matrix to which
#' dimnames are to be added. Fully specified by <M1> argument of
#' function {ds.matrixDimnames}. For more details
#' see help for {ds.matrixDimnames}.
#' @param dimnames A dimnames attribute for the matrix: NULL or a list of
#' length 2 giving the row and column names respectively.
#' Fully specified by <dimnames> argument of
#' function {ds.matrixDimnames}. For more details
#' see help for {ds.matrixDimnames}.
#' @return Output is the serverside matrix specified by the <newobj> argument
#' (or default name diag_<x1>) with specified dimnames (row and column
#' names) which is written to the serverside.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
matrixDimnamesDS <- function(M1.name=NULL,dimnames){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
nfilter.string<-as.numeric(thr$nfilter.string)              			#
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################

#Check length of M1.name not so long as to provide a risk of hidden code
length.M1.name<-length(unlist(strsplit(M1.name,'')))

if(length.M1.name>nfilter.stringShort)
	{
	studysideMessage<-
	paste0("FAILED: M1.name is too long it could hide concealed code, please shorten to <= nfilter.stringShort = ",
	       nfilter.stringShort," characters")
	stop(studysideMessage, call. = FALSE)
	}


#Check length of dimnames string not so long as to provide a risk of hidden code
#This could legitimately be quite long so allow 2*nfilter.string

if(!is.null(dimnames))
	{
      length.dimnames<-length(unlist(strsplit(paste(unlist(dimnames),collapse=''),'')))

		if(length.dimnames>(2*nfilter.string))
		{
		studysideMessage<-
		paste0("FAILED: dimnames is too long it could hide concealed code, please shorten to <= 2 x nfilter.string = ",
			(nfilter.string*2)," characters")
		stop(studysideMessage, call. = FALSE)
		}
	}

#EVAL M1

M1<-eval(parse(text=M1.name), envir = parent.frame())

if(!is.matrix(M1)&&!is.data.frame(M1))
	{
	studysideMessage<-"FAILED: M1 must be of class matrix or data.frame, please respecify"
	stop(studysideMessage, call. = FALSE)
	}

#coerce to matrix if a data.frame
if(is.data.frame(M1))
	{
	M1<-as.matrix(M1)
	}


matrix.output<-M1
dimnames(matrix.output)<-dimnames


return(matrix.output)
}

#ASSIGN FUNCTION
# matrixDimnamesDS
