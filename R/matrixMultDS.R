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
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
matrixMultDS <- function(M1.name=NULL, M2.name=NULL){

M1 <- .loadServersideObject(M1.name)
.checkClass(obj = M1, obj_name = M1.name, permitted_classes = c("matrix", "data.frame"))

M2 <- .loadServersideObject(M2.name)
.checkClass(obj = M2, obj_name = M2.name, permitted_classes = c("matrix", "data.frame"))

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
