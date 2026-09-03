#'
#' @title matrixInvertDS serverside assign function called by ds.matrixInvert
#' @description Inverts a square matrix A and writes the output to the serverside
#' @details Undertakes standard matrix inversion. This operation is only
#' possible if the number of columns and rows of A are the same and the matrix
#' is non-singular - positive definite (eg there is no row or column that is all zeros)
#' @param M1.name  A character string specifying the name of the matrix to be inverted
#' @return Output is the matrix representing the inverse of A which is written
#' to the serverside. For more details see help for ds.matrixInvert
#' @author Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
matrixInvertDS <- function(M1.name=NULL){

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

#Check can be inverted (not singular)
if(det(M1)==0.000)
	{
	studysideMessage<-"FAILED: matrix singular so cannot be inverted, please respecify"
	stop(studysideMessage, call. = FALSE)
	}



output<-solve(M1)


return(output)
}

#ASSIGN FUNCTION
# matrixInvertDS
