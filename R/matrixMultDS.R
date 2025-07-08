#'
#' @title matrixMultDS serverside assign function called by ds.matrixMult
#' @description Calculates the matrix product of two matrices and writes output to serverside
#' @details Undertakes standard matrix multiplication where with input matrices A and B with
#' dimensions A: mxn and B: nxp the output C has dimensions mxp and each element C[i,j] has
#' value equal to the dot product of row i of A and column j of B where the dot product
#' is obtained as sum(A[i,1]*B[1,j] + A[i,2]*B[2,j] + .... + A[i,n]*B[n,j]). This calculation
#' is only valid if the number of columns of A is the same as the number of rows of B
#' @param M1.name  A character string specifying the name of the first matrix (M1) argument
#' specified by the M1 argument in the original call to ds.matrixMult
#' @param M2.name  A character string specifying the name of the second matrix (M2) argument
#' specified by the M1 argument in the original call to ds.matrixMult
#' @return Output is the matrix representing the product of M1 and M2 which is written
#' to the serverside. For more details see help for ds.matrixMult
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
matrixMultDS <- function(M1.name=NULL, M2.name=NULL){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
thr<-dsBase::listDisclosureSettingsDS()                                 #
#nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
#nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
#nfilter.string<-as.numeric(thr$nfilter.string)                         #
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)                #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################
#EVAL M1 and M2

#Check length of M1.name not so long as to provide a risk of hidden code
length.M1.name<-length(unlist(strsplit(M1.name,'')))

if(length.M1.name>nfilter.stringShort)
	{
	studysideMessage<-
	paste0("FAILED: M1.name is too long it could hide concealed code, please shorten to <= nfilter.stringShort = ",
	       nfilter.stringShort," characters")
	stop(studysideMessage, call. = FALSE)
	}

#Check length of M2.name not so long as to provide a risk of hidden code
length.M2.name<-length(unlist(strsplit(M2.name,'')))

if(length.M2.name>nfilter.stringShort)
	{
	studysideMessage<-
	paste0("FAILED: M2.name is too long it could hide concealed code, please shorten to <= nfilter.stringShort = ",
	       nfilter.stringShort," characters")
	stop(studysideMessage, call. = FALSE)
	}

M1<-eval(parse(text=M1.name), envir = parent.frame())
M2<-eval(parse(text=M2.name), envir = parent.frame())



if(!is.matrix(M1)&&!is.data.frame(M1))
	{
	studysideMessage<-"FAILED: M1 must be of class matrix or data.frame, please respecify"
	stop(studysideMessage, call. = FALSE)
	}

if(!is.matrix(M2)&&!is.data.frame(M2))
	{
	studysideMessage<-"FAILED: M2 must be of class matrix or data.frame, please respecify"
	stop(studysideMessage, call. = FALSE)
	}

if(is.data.frame(M1))
	{
	M1<-as.matrix(M1)
	}

if(is.data.frame(M2))
	{
	M2<-as.matrix(M2)
	}



#Check dimensions valid
if(ncol(M1)!=nrow(M2))
	{
	studysideMessage<-"FAILED: invalid dimensions ncol(M1) must equal nrow(M2), please respecify"
	stop(studysideMessage, call. = FALSE)
	}


output<-M1%*%M2


return(output)
}

#ASSIGN FUNCTION
# matrixMultDS
