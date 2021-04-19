#' @title matrixDetDS assign function called by ds.matrixDet
#' @description Calculates the determinant of a square matrix A and writes
#' the output to the serverside
#' @details Calculates the determinant of a square matrix (for additional
#' information see help for {det} function in native R). This operation is only
#' possible if the number of columns and rows of A are the same.
#' @param M1.name  A character string specifying the name of the matrix for which
#' determinant to be calculated
#' @param logarithm logical. Default is FALSE, which returns the
#' determinant itself, TRUE returns the logarithm of the modulus of the determinant.
#' @return Output is the determinant of the matrix identified by argument <M1>
#' which is written to the serverside. For more details see help for ds.matrixDet
#' @author Paul Burton for DataSHIELD Development Team
#' @export

matrixDetDS2 <- function(M1.name=NULL,logarithm){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
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


#Check dimensions valid
if(ncol(M1)!=nrow(M1))
	{
	studysideMessage<-"FAILED: invalid dimensions M1 must be square: ncol must equal nrow, please respecify"
	stop(studysideMessage, call. = FALSE)
	}


output<-determinant(M1,logarithm=logarithm)



return(output)
}

#ASSIGN FUNCTION
# matrixDetDS2
