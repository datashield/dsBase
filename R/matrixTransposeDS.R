#'
#' @title matrixTransposeDS serverside assign function called by ds.matrixTranspose
#' @description Transposes a matrix A and writes the output to the serverside
#' @details Undertakes standard matrix transposition. This operation converts matrix
#' A to matrix C where element C[i,j] of matrix C equals element A[j,i] of matrix
#' A. Matrix A therefore has the same number of rows as matrix C has columns and
#' vice versa.
#' @param M1.name  A character string specifying the name of the matrix to be transposed
#' @return Output is the matrix representing the transpose of A which is written
#' to the serverside. For more details see help for ds.matrixTranspose
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
matrixTransposeDS <- function(M1.name=NULL){
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

 

output<-t(M1)


return(output)
}

#ASSIGN FUNCTION
# matrixTransposeDS
