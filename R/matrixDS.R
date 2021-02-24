#' @title matrixDS assign function called by ds.matrix
#' @description Creates a matrix A on the serverside
#' @details Similar to the {matrix()} function in native R. Creates a matrix
#' with dimensions specified by <nrows.scalar> and <ncols.scalar> arguments
#' and assigns the values of all its elements based on the <mdata> argument
#' @param mdata.transmit specifies the elements of the matrix to be created. Fully
#' specified by <mdata> argument of ds.matrix
#' @param from a character string specifying the source and nature of <mdata>.
#' Fully specified by <from> argument of ds.matrix
#' @param nrows.transmit specifies the number of rows in the matrix to be created.
#' Fully specified by <nrows.scalar> argument of ds.matrix
#' @param ncols.transmit specifies the number of columns in the matrix to be created.
#' Fully specified by <ncols.scalar> argument of ds.matrix
#' @param byrow a logical value specifying whether, when <mdata> is a vector,
#' the matrix created should be filled row by row or
#' column by column.
#' Fully specified by <byrow> argument of ds.matrix
#' @param dimnames A dimnames attribute for the matrix: NULL or a list of length 2 giving
#' the row and column names respectively. An empty list is treated as NULL,
#' and a list of length one as row names only.
#' Fully specified by <dimnames> argument of ds.matrix
#' @return Output is the matrix A written
#' to the serverside. For more details see help for ds.matrix
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
matrixDS <- function(mdata.transmit, from, nrows.transmit, ncols.transmit, byrow, dimnames){

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

#Check length of mdata.transmit string not so long as to provide a risk of hidden code
length.mdata.transmit<-length(strsplit(mdata.transmit,''))

if(length.mdata.transmit>nfilter.stringShort)
	{
	studysideMessage<-
	paste0("FAILED: mdata.transmit is too long it could hide concealed code, please shorten to <= nfilter.stringShort = ",
	       nfilter.stringShort," characters")
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

	#Check length of ncols.transmit string not so long as to provide a risk of hidden code
	length.ncols.transmit<-length(unlist(strsplit(ncols.transmit,'')))

if(length.ncols.transmit>nfilter.stringShort)
	{
	studysideMessage<-
	paste0("FAILED: ncols.transmit is too long it could hide concealed code, please shorten to <= nfilter.stringShort = ",
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


#Evaluate mdata.transmit via route depending on <from>

if(from=="serverside.vector"||from=="serverside.scalar")
{
mdata<-eval(parse(text=mdata.transmit), envir = parent.frame())
}


if(from=="clientside.scalar")
{
mdata.text<-strsplit(mdata.transmit, split=",")

mdata.c<-eval(parse(text=mdata.text), envir = parent.frame())

mdata<-as.numeric(mdata.c)
}


#Evaluate nrows.transmit

nrows.text<-strsplit(nrows.transmit, split=",")

nrows.c<-eval(parse(text=nrows.text), envir = parent.frame())

nrows<-as.numeric(nrows.c)


#Evaluate ncols.transmit

ncols.text<-strsplit(ncols.transmit, split=",")

ncols.c<-eval(parse(text=ncols.text), envir = parent.frame())

ncols<-as.numeric(ncols.c)


if(nrows==-9||ncols==-9)
	{
	studysideMessage<-"FAILED: must specify both nrows.scalar and ncols.scalar as positive integers"
	stop(studysideMessage, call. = FALSE)
	}


#NOW CONSIDER ALL CALL TYPES

		if(from=="serverside.vector")
		{
		output<-matrix(mdata,nrows,ncols,byrow,dimnames)
		}

		if(from=="serverside.scalar")
		{
		output<-matrix(mdata,nrows,ncols,byrow,dimnames)
		}

		if(from=="clientside.scalar")
		{
		output<-matrix(mdata,nrows,ncols,byrow,dimnames)
		}

  if(from!="serverside.vector"&&from!="serverside.scalar"&&from!="clientside.scalar")
	  {
			studysideMessage<-paste0("FAILED: the <from> argument specified is not valid, please respecify")
			stop(studysideMessage, call. = FALSE)
	  }

	return(output)
}

#ASSIGN FUNCTION
# matrixDS
