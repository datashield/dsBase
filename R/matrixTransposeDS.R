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
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
matrixTransposeDS <- function(M1.name=NULL){

M1 <- .loadServersideObject(M1.name)
.checkClass(obj = M1, obj_name = M1.name, permitted_classes = c("matrix", "data.frame"))

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
