###############################################################################|###################| 80 and 100

#' @title dataFrameSubsetDS2.o an assign function called by ds.dataFrameSubset.o
#' @description Second serverside function for subsetting a data frame by row or by column.
#' @details A data frame is a list of variables all with the same number of rows,
#' which is of class 'data.frame'. For all details see the help header for ds.dataFrameSubset.o
#' @param df.name a character string providing the name for the data.frame
#' to be sorted. <df.name> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @param V1.name A character string specifying the name of a subsetting vector
#' to which a Boolean operator will be applied to define the subset to be created.
#' <V1.name> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @param V2.name A character string specifying the name of the vector
#' or scalar to which the values in the vector specified by the argument <V1.name>
#' is to be compared.
#' <V2.name> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @param Boolean.operator.n A character string specifying one of six possible Boolean operators:
#' '==', '!=', '>', '>=', '<', '<='
#' <Boolean.operator.n> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @param keep.cols a numeric vector specifying the numbers of the columns to be kept in the
#' final subset when subsetting by column. For example: keep.cols=c(2:5,7,12) will keep
#' columns 2,3,4,5,7 and 12.
#' <keep.cols> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @param rm.cols a numeric vector specifying the numbers of the columns to be removed before
#' creating the final subset when subsetting by column. For example: rm.cols=c(2:5,7,12)
#' will remove columns 2,3,4,5,7 and 12.
#' <rm.cols> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @param keep.NAs logical, if TRUE any NAs in the vector holding the final Boolean vector
#' indicating whether a given row should be included in the subset will be converted into
#' 1s and so they will be included in the subset. Such NAs could be caused by NAs in
#' either <V1.name> or <V2.name>. If FALSE or NULL NAs in the final Boolean vector will
#' be converted to 0s and the corresponding row will therefore be excluded from the subset.
#' <keep.NAs> argument generated and passed directly to
#' dataFrameSubsetDS2 by ds.dataFrameSubset.o
#' @return the object specified by the <newobj> argument (or default name '<df.name>_subset')
#' initially specified in calling ds.dataFrameSubset.o. The output object (the required
#' subsetted data.frame called <newobj> is written to the serverside. In addition,
#' two validity messages are returned via ds.dataFrameSubset.o
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' dataFrameSubsetDS2.o (via ds.dataFrame.o()) also returns any studysideMessages
#' that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export
#'
dataFrameSubsetDS2.o<-function(df.name=NULL,V1.name=NULL, V2.name=NULL, Boolean.operator.n=NULL,keep.cols=NULL, rm.cols=NULL, keep.NAs=NULL){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr <- listDisclosureSettingsDS.o()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
nfilter.string<-as.numeric(thr$nfilter.string)              			#
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
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

                      ###############################################################################|###################| 80 and 100
if(sum(is.na(keep.code.n))>0){
   studysideMessage<-"FAILED: keep.cols argument contains non-numerics (disclosure risk)"
   return(list(studysideMessage=studysideMessage))
}else{

keep.cols<-keep.code.n
}

#Code must only contain numeric elements split by ",",
#anything else will fail outright or will have returned an NA to
#code.num and so the following sum will exceed 0

if(sum(is.na(keep.code.n))>0){
   studysideMessage<-"FAILED: keep.cols argument contains non-numerics (disclosure risk)"
   return(list(studysideMessage=studysideMessage))
}else{

keep.cols<-keep.code.n
}

}
#ARGUMENTS TO BE EVALUATED	
#rm.cols
if(!is.null(rm.cols)){

rm.code.input<-rm.cols
rm.code.c<-unlist(strsplit(rm.code.input, split=","))
rm.code.n<-as.numeric(rm.code.c)

#In this case, code must only contain numeric elements split by ",",
#anything else will fail outright or will have returned an NA to
#code.num and so the following sum will exceed 0

if(sum(is.na(rm.code.n))>0){
   studysideMessage<-"FAILED: rm.cols argument contains non-numerics (disclosure risk)"
   return(list(studysideMessage=studysideMessage))
}else{

rm.cols<-rm.code.n
}

#Code must only contain numeric elements split by ",",
#anything else will fail outright or will have returned an NA to
#code.num and so the following sum will exceed 0

if(sum(is.na(rm.code.n))>0){
   studysideMessage<-"FAILED: rm.cols argument contains non-numerics (disclosure risk)"
   return(list(studysideMessage=studysideMessage))
}else{

rm.cols<-rm.code.n
}
}



#ADDITIONAL DISCLOSURE TRAPS

if(!is.null(df.name)){
   df.name.chars<-strsplit(df.name,split="")
   if(length(df.name.chars[[1]])>nfilter.string){
      studysideMessage<-"FAILED: df.name argument > nfilter.string - please shorten"
	   return(list(studysideMessage=studysideMessage))
  }
}

if(!is.null(V1.name)){
  V1.name.chars<-strsplit(V1.name,split="")
  if(length(V1.name.chars[[1]])>nfilter.string){

   studysideMessage<-"FAILED: V[i].name argument > nfilter.string - please shorten"
   return(list(studysideMessage=studysideMessage))
  }
}

if(!is.null(V2.name)){
  V2.name.chars<-strsplit(V2.name,split="")
  if(length(V2.name.chars[[1]])>nfilter.string){
   studysideMessage<-"FAILED: V[ii].name argument > nfilter.string - please shorten"
   return(list(studysideMessage=studysideMessage))
  }
}

 
   df.name.2<-paste0("data.frame(",df.name,")")
   df2subset <- eval(parse(text=df.name.2))

   V1<-eval(parse(text=V1.name))
   
   V2<-eval(parse(text=V2.name))
   


##########CHECK APPROPRIATE CLASSES ##############
if(!is.character(df.name) || !is.data.frame(df2subset)){
   studysideMessage<-"FAILED: df.name argument must be character and must name a data.frame"
   return(list(studysideMessage=studysideMessage))
   }

if(!is.character(V1.name)){
   studysideMessage<-"FAILED: V[i].name must be character"
   return(list(studysideMessage=studysideMessage))
   }

if(!is.character(V2.name)){
   studysideMessage<-"FAILED: V[ii].name must be character"
   return(list(studysideMessage=studysideMessage))
   }



########### CHECK LENGTHS OF V1, V2 ARE CONSISTENT WITH COLUMN LENGTH OF df TO BE SUBSETTED
df.col.length<-dim(df2subset)[1]
V1.length<-length(V1)   
V2.length<-length(V2)

if(!((df.col.length == V1.length))){

                      ###############################################################################|###################| 80 and 100

   studysideMessage<-"FAILED: V[i] must of length equal to column length of df being subsetted"
   return(list(studysideMessage=studysideMessage))
}

if(!((V1.length == V2.length) | (V2.length==1))){
   studysideMessage<-"FAILED: V[ii] must either be of length one or of length equal to V[i]"

   return(list(studysideMessage=studysideMessage))
}

if(!is.numeric(Boolean.operator.n) | Boolean.operator.n==0){
   studysideMessage<-"FAILED: Boolean.operator must be: '==', '!=', '<', '<=', '>' or '>='"
   return(list(studysideMessage=studysideMessage))
}

Boolean.operator<-"  "
if(Boolean.operator.n==1) Boolean.operator<-"=="
if(Boolean.operator.n==2) Boolean.operator<-"!="
if(Boolean.operator.n==3) Boolean.operator<-"<"
if(Boolean.operator.n==4) Boolean.operator<-"<="
if(Boolean.operator.n==5) Boolean.operator<-">"
if(Boolean.operator.n==6) Boolean.operator<-">="


#APPLY BOOLEAN OPERATOR SPECIFIED

Boolean.indicator<-integer(length=V1.length)

#EVALUATE DIFFERENTLY IF V2 IS SAME LENGTH AS V1 OR OF LENGTH 1
if(V2.length==V1.length){
for(j in 1:V1.length){
command.text<-paste0(V1.name,"[",j,"]",Boolean.operator,V2.name,"[",j,"]")
Boolean.indicator[j]<-eval(parse(text=command.text))*1
}
}

if(V2.length==1){
for(j in 1:V1.length){
command.text<-paste0(V1.name,"[",j,"]",Boolean.operator,V2.name)
Boolean.indicator[j]<-eval(parse(text=command.text))*1
}
}

#BY DEFAULT IF SELECTION VARIABLE HAS MISSING VALUES EXPLICITLY REPLACE NAs WITH 0
#TO DISAMBIGUATE WHAT HAPPENS BUT IF keep.NAs IS REPLACE NAs WITH 1s (TO KEEP IN)
if(keep.NAs){
Boolean.indicator[is.na(Boolean.indicator)==1]<-1
}else{
Boolean.indicator[is.na(Boolean.indicator)==1]<-0
}

#NOW SUBSET df TO BE SUBSETTED
df.subset<-df2subset[(Boolean.indicator==1),]

#CHECK SUBSET LENGTH IS CONSISTENT WITH nfilter FOR MINIMUM SUBSET SIZE
subset.size<-dim(df.subset)[1]

if(subset.size < nfilter.subset){
   studysideMessage<-"Subset to be created is too small (<nfilter.subset)"
   return(list(studysideMessage=studysideMessage))
  }

############### CHECK ONLY keep.cols OR rm.cols ARE SET: NOT BOTH ##########################

if(!is.null(keep.cols) && !is.null(rm.cols)){
   studysideMessage<-"You can either specify keep.cols or rm.cols, not both"
   return(list(studysideMessage=studysideMessage))
  }


#REMOVE COLUMNS BY SPECIFYING COLUMNS TO KEEP OR COLUMNS TO REMOVE
if(!is.null(keep.cols)){
    df.subset<-df.subset[,keep.code.n]
    return(df.subset)
}

if(!is.null(rm.cols)){
 
 numcols<-dim(df2subset)[2]
 template.cols<-c(1:numcols,rm.code.n)
 element.counts<-table(template.cols)
 element.counts<-2-element.counts
 keep.cols<-rep(1:numcols,element.counts)
 
    df.subset<-df.subset[,keep.cols]
    return(df.subset)
}

return(df.subset)

}
#ASSIGN FUNCTION
# dataFrameSubsetDS2.o
