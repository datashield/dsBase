#' @title rbindDS.o called by ds.rbind.o
#' @description serverside assign function that takes a
#' sequence of vector, matrix or data-frame arguments
#' and combines them by row to produce a matrix.
#' @details A sequence of vector, matrix or data-frame arguments
#' is combined row by row to produce a matrix
#' which is written to the serverside. For more details see 
#' help for {ds.rbind.o} and the native R function {rbind}.
#' @param x.names.transmit This is a vector of character strings
#' representing the names of the elemental
#' components to be combined converted into a transmittable
#' format. This argument is fully specified by the <x> argument
#' of {ds.rbind.o}
#' @param colnames.transmit This is NULL or a vector of character
#' strings representing forced column names for the output object 
#' converted into a transmittable format. This argument is fully
#' specified by the <force.colnames> argument
#' of {ds.cbind.o}.
#' @return the object specified by the <newobj> argument 
#' of {ds.rbind.o}(or default name <rbind.out>)
#' which is written to the serverside. As well as writing the output object as <newobj>
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
rbindDS.o<-function(x.names.transmit=NULL,colnames.transmit=NULL){
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
  
  colnames.input<-colnames.transmit
  colnames.active<-unlist(strsplit(colnames.input, split=","))
  
  x.names.input<-x.names.transmit
  x.names.active<-unlist(strsplit(x.names.input, split=","))
  numobj<-length(x.names.active)

  
  rbind.matrix<-NULL
  for(k in numobj:1){
    object.2.rbind<-eval(parse(text=x.names.active[k]))
    
    #coerce all input objects to data.matrix (like as.matrix but stays as numeric if numeric)
    object.2.rbind<-data.matrix(object.2.rbind)
    
    required.length.colnames.vector<-ncol(object.2.rbind)
    colnames.2.use<-colnames.active[1:required.length.colnames.vector]
    colnames(object.2.rbind)<-colnames.2.use
    rbind.matrix<-rbind(object.2.rbind,rbind.matrix)
  }
  colnames(rbind.matrix)<-colnames.2.use
  
  outobj<-rbind.matrix
  
  return(outobj)
}  

# ASSIGN FUNCTION
# rbindDS.o
