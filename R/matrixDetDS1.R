#' @title matrixDetDS aggregate function called by ds.matrixDet.report
#' @description Calculates the determinant of a square matrix A and returns
#' the output to the clientside
#' @details Calculates the determinant of a square matrix (for additional
#' information see help for \code{det} function in native R). This operation is only
#' possible if the number of columns and rows of A are the same.
#' @param M1.name  A character string specifying the name of the matrix for which
#' determinant to be calculated
#' @param logarithm logical. Default is FALSE, which returns the
#' determinant itself, TRUE returns the logarithm of the modulus of the determinant.
#' @return Output is the determinant of the matrix identified by argument <M1>
#' which is returned to the clientside. For more details see help for ds.matrixDet
#' @author Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export

matrixDetDS1 <- function(M1.name=NULL,logarithm){

dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'avocado', 'banana'))

thr <- dsBase::listDisclosureSettingsDS()
nfilter.subset <- as.numeric(thr$nfilter.subset)

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
	error.message<-"FAILED: invalid dimensions M1 must be square: ncol must equal nrow, please respecify"
	stop(error.message, call. = FALSE)
	}

#Check matrix large enough to reduce disclosure risk
if(nrow(M1)<nfilter.subset)
	{
	error.message<-"FAILED: matrix is too small (nrows < nfilter.subset), please respecify"
	stop(error.message, call. = FALSE)
	}

output<-determinant(M1,logarithm=logarithm)



return(list(matrix.determinant=output))
}

#AGGREGATE FUNCTION
# matrixDetDS1
