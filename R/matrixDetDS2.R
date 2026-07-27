#' @title matrixDetDS assign function called by ds.matrixDet
#' @description Calculates the determinant of a square matrix A and writes
#' the output to the serverside
#' @details Calculates the determinant of a square matrix (for additional
#' information see help for \code{det} function in native R). This operation is only
#' possible if the number of columns and rows of A are the same.
#' @param M1.name  A character string specifying the name of the matrix for which
#' determinant to be calculated
#' @param logarithm logical. Default is FALSE, which returns the
#' determinant itself, TRUE returns the logarithm of the modulus of the determinant.
#' @return Output is the determinant of the matrix identified by argument <M1>
#' which is written to the serverside. For more details see help for ds.matrixDet
#' @author Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export

matrixDetDS2 <- function(M1.name=NULL,logarithm){

M1 <- .loadServersideObject(M1.name)
.checkClass(obj = M1, obj_name = M1.name, permitted_classes = c("matrix", "data.frame"))

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
