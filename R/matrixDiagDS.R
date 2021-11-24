#' @title matrixDiagDS assign function called by ds.matrixDiag
#' @description Extracts the diagonal vector from a square matrix A or
#' creates a diagonal matrix A based on a vector or a scalar value and
#' writes the output to the serverside
#' @details For details see help for function {ds.matrixDiag}.
#' @param x1.transmit identifies the input matrix or vector. Fully
#' specified by <x1> argument of {ds.matrixDiag}. For more details
#' see help for {ds.matrixDiag}.
#' @param aim a character string specifying what behaviour is required
#' of the function. Fully specified by <aim> argument of {ds.matrixDiag}.
#' For more details see help for {ds.matrixDiag}.
#' @param nrows.transmit a scalar value forcing the number of rows and
#' columns in an output matrix.Fully specified by <nrows.scalar>
#' argument of {ds.matrixDiag}.
#' For more details see help for {ds.matrixDiag}.
#' @return Output is the matrix or vector specified by the <newobj> argument
#' (or default name diag_<x1>) which is written to the serverside.
#' For more details see help for {ds.matrixDiag}.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
matrixDiagDS <- function(x1.transmit,aim,nrows.transmit){
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

#Check length of x1.transmit string not so long as to provide a risk of hidden code
length.x1.transmit<-length(strsplit(x1.transmit,''))


#As a diagonal vector could potentially be quite long
#allow a substantial number of characters

nfilter.string.long<-5*nfilter.string

if(length.x1.transmit>nfilter.string.long)
	{
	studysideMessage<-
	paste0("FAILED: x1.transmit is too long it could hide concealed code, please shorten to <= nfilter.string*5 = ",
	       nfilter.string.long," characters")
	stop(studysideMessage, call. = FALSE)
	}

#Check length of nrows.transmit string not so long as to provide a risk of hidden code
length.nrows.transmit<-length(unlist(strsplit(nrows.transmit,'')))

if(length.nrows.transmit>nfilter.stringShort)
	{
	studysideMessage<-
	paste0("FAILED: nrows.transmit is too long it could hide concealed code, please shorten to <= nfilter.stringShort = ",
	       nfilter.stringShort," characters")
	stop(studysideMessage, call. = FALSE)
	}

#Evaluate x1.transmit via route depending on aim

if(aim=="serverside.vector.2.matrix"||aim=="serverside.scalar.2.matrix"||aim=="serverside.matrix.2.vector")
{
#x1 is name of the serverside vector, scalar or matrix

x1<-eval(parse(text=x1.transmit), envir = parent.frame())

#coerce to matrix if x1 is a data.frame
if(is.data.frame(x1))
	{
	x1<-as.matrix(x1)
	}
}


if(aim=="clientside.vector.2.matrix"||aim=="clientside.scalar.2.matrix")
{
x1.text<-strsplit(x1.transmit, split=",")

x1.c<-eval(parse(text=x1.text), envir = parent.frame())

x1<-as.numeric(x1.c)
}


#Evaluate nrows.transmit

nrows.text<-strsplit(nrows.transmit, split=",")

nrows.c<-eval(parse(text=nrows.text), envir = parent.frame())

nrows<-as.numeric(nrows.c)




#NOW CONSIDER ALL CALL TYPES

		if(aim=="serverside.vector.2.matrix"||aim=="serverside.matrix.2.vector"||aim=="clientside.vector.2.matrix")
		{
		output<-diag(x1)
		}




		if(aim=="serverside.scalar.2.matrix"||aim=="clientside.scalar.2.matrix")
		{
		if(nrows==-9)
			{
			studysideMessage<-
			paste0("FAILED: if x1 is a scalar you must specify argument <nrows> as an integer to fix matrix dimensions")
			stop(studysideMessage, call. = FALSE)
			}
			else
			{
			output<-diag(x1,nrows)
			}
		}

  if(aim!="serverside.vector.2.matrix"&&aim!="serverside.scalar.2.matrix"&&aim!="serverside.matrix.2.vector"&&
      aim!="clientside.vector.2.matrix"&&aim!="clientside.scalar.2.matrix")
	  {
			studysideMessage<-paste0("FAILED: the aim specified is not valid, please respecify")
			stop(studysideMessage, call. = FALSE)
	  }

	return(output)
}

#ASSIGN FUNCTION
# matrixDiagDS
