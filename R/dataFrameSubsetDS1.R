#'
#' @title dataFrameSubsetDS1 an aggregate function called by ds.dataFrameSubset
#' @description First serverside function for subsetting a data frame by row or by column.
#' @details A data frame is a list of variables all with the same number of rows,
#' which is of class 'data.frame'. For all details see the help header for ds.dataFrameSubset
#' @param df.name a character string providing the name for the data.frame
#' to be sorted. <df.name> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @param V1.name A character string specifying the name of a subsetting vector
#' to which a Boolean operator will be applied to define the subset to be created.
#' <V1.name> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @param V2.name A character string specifying the name of the vector
#' or scalar to which the values in the vector specified by the argument <V1.name>
#' is to be compared.
#' <V2.name> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @param Boolean.operator.n A character string specifying one of six possible Boolean operators:
#' '==', '!=', '>', '>=', '<', '<='
#' <Boolean.operator.n> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @param keep.cols a numeric vector specifying the numbers of the columns to be kept in the
#' final subset when subsetting by column. For example: keep.cols=c(2:5,7,12) will keep
#' columns 2,3,4,5,7 and 12.
#' <keep.cols> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @param rm.cols a numeric vector specifying the numbers of the columns to be removed before
#' creating the final subset when subsetting by column. For example: rm.cols=c(2:5,7,12)
#' will remove columns 2,3,4,5,7 and 12.
#' <rm.cols> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @param keep.NAs logical, if TRUE any NAs in the vector holding the final Boolean vector
#' indicating whether a given row should be included in the subset will be converted into
#' 1s and so they will be included in the subset. Such NAs could be caused by NAs in
#' either <V1.name> or <V2.name>. If FALSE or NULL NAs in the final Boolean vector will
#' be converted to 0s and the corresponding row will therefore be excluded from the subset.
#' <keep.NAs> argument generated and passed directly to
#' dataFrameSubsetDS1 by ds.dataFrameSubset
#' @return This first serverside function called by ds.dataFrameSubset provides
#' first level traps for a comprehensive series of disclosure risks which can be
#' returned directly to the clientside because dataFrameSubsetDS1 is an aggregate
#' function. The second serverside function called by ds.dataFrameSubset
#' (dataFrameSubsetDS2) carries out most of the same disclosure tests, but it is
#' an assign function because it writes the subsetted data.frame to the serverside.
#' In consequence, it records error messages as studysideMessages which can only be
#' retrieved using ds.message
#' @author Paul Burton
#' @export
#'
dataFrameSubsetDS1 <- function(df.name=NULL,V1.name=NULL,V2.name=NULL,Boolean.operator.n=NULL,keep.cols=NULL,rm.cols=NULL,keep.NAs=NULL){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
thr <- listDisclosureSettingsDS()
#nfilter.tab<-as.numeric(thr$nfilter.tab)
#nfilter.glm<-as.numeric(thr$nfilter.glm)
nfilter.subset <- as.numeric(thr$nfilter.subset)
nfilter.string <- as.numeric(thr$nfilter.string)
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)
#########################################################################

###############################################
#SCRIPT.TO.CHECK.VALIDITY.OF.EVALUATABLE.RCODE#
###############################################

#ARGUMENTS TO BE EVALUATED
#keep.cols
if(!is.null(keep.cols)){

keep.code.input<-keep.cols
keep.code.c<-unlist(strsplit(keep.code.input, split=","))
keep.code.n<-as.numeric(keep.code.c)

#In this case, code must only contain numeric elements split by ",",
#anything else will fail outright or will have returned an NA to
#code.num and so the following sum will exceed 0

if(sum(is.na(keep.code.n))>0){
   studysideMessage<-"FAILED: keep.cols argument contains non-numerics (disclosure risk)"
   stop(studysideMessage, call. = FALSE)
}else{

keep.cols<-keep.code.n
}

#Code must only contain numeric elements split by ",",
#anything else will fail outright or will have returned an NA to
#code.num and so the following sum will exceed 0

if(sum(is.na(keep.code.n))>0){
   studysideMessage <- "FAILED: keep.cols argument contains non-numerics (disclosure risk)"
   stop(studysideMessage, call. = FALSE)
}else{
  keep.cols <- keep.code.n
}

}
#ARGUMENTS TO BE EVALUATED
#rm.cols
  if(!is.null(rm.cols)){

    rm.code.input <- rm.cols
    rm.code.c <- unlist(strsplit(rm.code.input, split=","))
    rm.code.n <- as.numeric(rm.code.c)

  #In this case, code must only contain numeric elements split by ",",
  #anything else will fail outright or will have returned an NA to
  #code.num and so the following sum will exceed 0

    if(sum(is.na(rm.code.n))>0){
      studysideMessage <- "FAILED: rm.cols argument contains non-numerics (disclosure risk)"
      stop(studysideMessage, call. = FALSE)
    }else{
      rm.cols <- rm.code.n
    }

  #Code must only contain numeric elements split by ",",
  #anything else will fail outright or will have returned an NA to
  #code.num and so the following sum will exceed 0

    if(sum(is.na(rm.code.n))>0){
      studysideMessage <- "FAILED: rm.cols argument contains non-numerics (disclosure risk)"
      stop(studysideMessage, call. = FALSE)
    }else{
      rm.cols <- rm.code.n
    }
  }

  # ADDITIONAL DISCLOSURE TRAPS

  if(!is.null(df.name)){
    df.name.chars <- strsplit(df.name,split="")
    if(length(df.name.chars[[1]])>nfilter.string){
      studysideMessage <- "FAILED: df.name argument > nfilter.string - please shorten"
      stop(studysideMessage, call. = FALSE)
    }
  }

  if(!is.null(V1.name)){
    V1.name.chars <- strsplit(V1.name,split="")
    if(length(V1.name.chars[[1]])>nfilter.string){
      studysideMessage <- "FAILED: V[i].name argument > nfilter.string - please shorten"
      stop(studysideMessage, call. = FALSE)
    }
  }

  if(!is.null(V2.name)){
    V2.name.chars <- strsplit(V2.name,split="")
    if(length(V2.name.chars[[1]])>nfilter.string){
      studysideMessage <- "FAILED: V[ii].name argument > nfilter.string - please shorten"
      stop(studysideMessage, call. = FALSE)
    }
  }

  df.name.2 <- paste0("data.frame(",df.name,")")
  df2subset <- eval(parse(text=df.name.2))

  if(V1.name=="ONES"||V2.name=="ONES")
  {
    length.ONES<-dim(df2subset)[1]
    V1<-rep(1,length=length.ONES)
    V2<-rep(1,length=length.ONES)
    Boolean.operator.n<-1
    #if using "ONES" for V1 or V2 then need to ensure a variable called "ONES" exists
    #when it comes to generating the Boolean indicator below. If it doesn't exist
    #generate it. If it does exist (for another purpose) then just leave as it is
    #because its form doesn't matter, it just has to exist
    if(!exists("ONES"))
    {
      ONES<-V1
    }
  } else {
     V1 <- eval(parse(text=V1.name))
     V2 <- eval(parse(text=V2.name))
  }

  ##########CHECK APPROPRIATE CLASSES ##############
  if(!is.character(df.name) || !is.data.frame(df2subset)){
    studysideMessage <- "FAILED: df.name argument must be character and must name a data.frame"
    stop(studysideMessage, call. = FALSE)
  }

  if(!is.character(V1.name)){
    studysideMessage <- "FAILED: V[i].name must be character"
    stop(studysideMessage, call. = FALSE)
  }

  if(!is.character(V2.name)){
    studysideMessage <- "FAILED: V[ii].name must be character"
    stop(studysideMessage, call. = FALSE)
  }

  ########### CHECK LENGTHS OF V1, V2 ARE CONSISTENT WITH COLUMN LENGTH OF df TO BE SUBSETTED
  df.col.length <- dim(df2subset)[1]
  V1.length <- length(V1)
  V2.length <- length(V2)

  if(!((df.col.length == V1.length))){
    studysideMessage<-"FAILED: V[i] must of length equal to column length of df to be subsetted"
    stop(studysideMessage, call. = FALSE)
  }

  if(!((V1.length == V2.length) || (V2.length==1))){
    studysideMessage<-"FAILED: V[ii] must either be of length one or of length equal to V[i]"
    stop(studysideMessage, call. = FALSE)
  }

  if(!is.numeric(Boolean.operator.n) || Boolean.operator.n==0){
    studysideMessage <- "FAILED: Boolean.operator must be: '==', '!=', '<', '<=', '>' or '>='"
    stop(studysideMessage, call. = FALSE)
  }

  Boolean.operator <- "  "
  if(Boolean.operator.n==1) Boolean.operator<-"=="
  if(Boolean.operator.n==2) Boolean.operator<-"!="
  if(Boolean.operator.n==3) Boolean.operator<-"<"
  if(Boolean.operator.n==4) Boolean.operator<-"<="
  if(Boolean.operator.n==5) Boolean.operator<-">"
  if(Boolean.operator.n==6) Boolean.operator<-">="

  #APPLY BOOLEAN OPERATOR SPECIFIED
  Boolean.indicator <- integer(length=V1.length)

  # EVALUATE DIFFERENTLY IF V2 IS SAME LENGTH AS V1 OR OF LENGTH 1
  if(V2.length==V1.length){
    for(j in 1:V1.length){
      command.text <- paste0(V1.name,"[",j,"]",Boolean.operator,V2.name,"[",j,"]")
      Boolean.indicator[j] <- eval(parse(text=command.text))*1
    }
  }
  if(V2.length==1){
    for(j in 1:V1.length){
      command.text <- paste0(V1.name,"[",j,"]",Boolean.operator,V2.name)
      Boolean.indicator[j] <- eval(parse(text=command.text))*1
    }
  }

  # BY DEFAULT IF SELECTION VARIABLE HAS MISSING VALUES EXPLICITLY REPLACE NAs WITH 0
  # TO DISAMBIGUATE WHAT HAPPENS BUT IF keep.NAs IS REPLACE NAs WITH 1s (TO KEEP IN)
  if(keep.NAs){
    Boolean.indicator[is.na(Boolean.indicator)==1]<-1
  }else{
    Boolean.indicator[is.na(Boolean.indicator)==1]<-0
  }

  # NOW SUBSET df TO BE SUBSETTED
  df.subset <- df2subset[(Boolean.indicator==1),]

  # CHECK SUBSET LENGTH IS CONSISTENT WITH nfilter FOR MINIMUM SUBSET SIZE
  subset.size <- dim(df.subset)[1]

  if(subset.size < nfilter.subset){
    studysideMessage <- "Subset to be created is too small (<nfilter.subset)"
    stop(studysideMessage, call. = FALSE)
  }

  # DISCLOSURE TRAP ON LENGTH OF dim(1) OF ORIGINAL DATA FRAME AND NEW SUBSET
  df.dim1.original <- dim(df2subset)[1]
  df.dim1.subset <- dim(df.subset)[1]
  difference.dim1s <- abs(df.dim1.subset-df.dim1.original)

  ########################################################################
  ##########MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
  ########################################################################

  if((difference.dim1s<nfilter.subset)&&(difference.dim1s>0)){
    studysideWarning1<-"Warning: DataSHIELD monitors every session for potentially disclosive analytic requests."
    studysideWarning2<-"The analysis you just submitted has generated a subset in which the number of elements"
    studysideWarning3<-"differs - but only very slightly so - from the original data frame. This is most likely to be"
    studysideWarning4<-"an innocent consequence of your subsetting needs. However, it could in theory be one step"
    studysideWarning5<-"in a difference-based attack aimed at identifying individuals. This analytic request has"
    studysideWarning6<-"therefore been highlighted in the session log file. Please be reassured, if you do not try"
    studysideWarning7<-"to identify individuals this will cause you no difficulty. However, if you do plan a "
    studysideWarning8<-"malicious attempt to identify individuals by differencing, this will become obvious in the"
    studysideWarning9<-"session log and you will be sanctioned. Possible consequences include loss of future access"
    studysideWarning10<-"to DataSHIELD and/or legal penalties."

    return.message <- list(studysideWarning1,studysideWarning2,studysideWarning3,studysideWarning4,
                   studysideWarning5,studysideWarning6,studysideWarning7,studysideWarning8,
				   studysideWarning9,studysideWarning10)
  }else{
    return.message <- "Subsetting undertaken without problems"
  }

########################################################################
##########MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
########################################################################

  return(return.message)

}
# AGGREGATE FUNCTION
# dataFrameSubsetDS1
