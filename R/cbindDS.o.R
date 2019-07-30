#' @title cbindDS.o called by ds.cbind.o c
#' @description serverside assign function that takes a
#' sequence of vector, matrix or data-frame arguments
#' and combines them by column to produce a matrix.
#' @details A sequence of vector, matrix or data-frame arguments
#' is combined column by column to produce a matrix
#' which is written to the serverside. For more details see 
#' help for {ds.cbind.o} and the native R function {cbind}.
#' @param x.names.transmit This is a vector of character strings
#' representing the names of the elemental
#' components to be combined converted into a transmittable
#' format. This argument is fully specified by the <x> argument
#' of {ds.cbind.o}
#' @param colnames.transmit This is NULL or a vector of character
#' strings representing forced column names for the output object 
#' converted into a transmittable format. This argument is fully
#' specified by the <force.colnames> argument
#' of {ds.cbind.o}.
#' @return the object specified by the <newobj> argument 
#' of {ds.cbind.o}(or default name <cbind.out>)
#' which is written to the serverside. Just like the {cbind} function in
#' native R, the output object is of class matrix unless one or more
#' of the input objects is a data.frame in which case the class of the
#' output object is data.frame. As well as writing the output object as <newobj>
#' on the serverside, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.cbind.o() also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("<newobj>") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("<newobj>")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team
#' @export
cbindDS.o <- function(x.names.transmit=NULL,colnames.transmit=NULL){
#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
#thr<-listDisclosureSettingsDS.o()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)        #
#########################################################################

x.names.input<-x.names.transmit
x.names.act1<-unlist(strsplit(x.names.input, split=","))
x.names.act2<-paste(x.names.act1,collapse=",")

eval.code.x.names<-paste0("cbind(",x.names.act2,")")

output.cbind<-eval(parse(text=eval.code.x.names))

colnames.input<-colnames.transmit
colnames.act1<-unlist(strsplit(colnames.input, split=","))

#Check length of colnames vector
required.length.colnames.vector<-ncol(output.cbind)
generated.length.colnames.vector<-length(colnames.act1)


if(required.length.colnames.vector!=generated.length.colnames.vector){
rescue.names.vector<-paste0("V",as.character(1:required.length.colnames.vector))
studysideMessage<-paste0("Number of column names does not match number of columns in output object. Here ", required.length.colnames.vector, " names are required. Please see help for {ds.cbind.o} function")
return(list(studysideMessage=studysideMessage))
}

colnames(output.cbind)<-colnames.act1

output.obj<-output.cbind

return(output.obj)
}  
 
# ASSIGN FUNCTION
# cbindDS.o
