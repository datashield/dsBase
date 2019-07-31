#' @title rmDS.o an aggregate function called by ds.rm.o
#' @description deletes an R object on the serverside  
#' @details this is a serverside function
#' based on the rm() function in native R. It is an aggregate function
#' which may be surprising because it modifies an object
#' on the serverside, and would therefore be expected to be an assign function.
#' However, as an assign function the last step in running it
#' would be to write the modified object as newobj. But this would
#' fail because the effect of the function is to delete the object and so
#' it would be impossible to write it anywhere.
#' @param x.name.transmit, the name of the object to be deleted converted
#' into transmissable form. The argument is specified via the <x.name>
#' argument of ds.rm.o
#' @return the specified object is deleted from the serverside. If this
#' is successful the message "Object <x.name> successfully deleted" is returned
#' to the clientside (where x.name is the name of the object to be deleted).
#' If the object to be deleted is already absent on a given
#' source, that source will return the message: "Object to be deleted, i.e. <x.name> ,
#' does not exist so does not need deleting". Finally, if the specified name
#' of the object to be deleted is too long (>nfilter.stringShort) there is
#' a potential disclosure risk (active code hidden in the name) and the
#' rmDS.o returns a message such as: "Disclosure risk, number of characters
#' in x.name must not exceed nfilter.stringShort which is currently set at: 25" where
#' '25' is the current setting of the R_Option value of nfilter.stringShort.
#' @author Paul Burton for DataSHIELD Development Team 
#' @export
rmDS.o <- function(x.name.transmit)
{
#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS.o()							#
#nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
#nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)        #
#########################################################################

#manage x.name

#check text to be activated is not too long because of disclosure risk
x.name.numchars<-length(unlist(strsplit(x.name.transmit,split="")))

if(x.name.numchars>nfilter.stringShort){
   return.message<-
   paste0("Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: ",nfilter.stringShort)
    return(list(return.message=return.message))
}

#convert x.name format from transmittable to actionable form (a vector of character strings)
	x.name<-unlist(strsplit(x.name.transmit, split=","))

	#tests whether already exists
test.already.exists<-exists(x.name)

if(!test.already.exists){
   return.message<-
   paste0("Object to be deleted, i.e. <",x.name, "> , does not exist so does not need deleting")
    return(list(return.message=return.message))
}

rm(list=c(x.name),pos=1)

#test whether still exists
test.still.exists<-exists(x.name)

if(test.still.exists){
   return.message<-paste0("Object to be deleted, i.e. <",x.name, "> , still exists")
	    return(list(return.message=return.message))

	}else{
		return.message<-paste0("Object <",x.name, "> successfully deleted")
	    return(list(return.message=return.message))
	}


return(return.message=return.message,x.name=x.name)
}
#AGGREGATE FUNCTION
# rmDS.o

