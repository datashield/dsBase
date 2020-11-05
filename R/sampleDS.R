#' @title random sampling and permuting of vectors, dataframes and matrices
#' @description draws a pseudorandom sample from a vector, dataframe or matrix
#' on the serverside
#' or - as a special case - randomly permutes a vector, dataframe or matrix.
#' @details Serverside assign function sampleDS called by clientside
#' function ds.sample. Based on the native R function {sample()} but deals
#' slightly differently with data.frames and matrices. For further details see 
#' help for ds.sample and native R help for sample().
#' @param x.transmit Either a character string providing the name for the serverside
#' vector, matrix or data.frame to be sampled or permuted, or an integer/numeric
#' scalar (e.g. 923) indicating that one should create a new vector on the serverside
#' that is a randomly permuted sample of the vector 1:923. x.transmit is
#' fully specified by the [x] argument of ds.sample. For further details see 
#' help for ds.sample and native R help for sample().
#' @param size.transmit a numeric/integer scalar indicating the size of the sample to
#' be drawn. size.transmit is fully specified by the [size] argument of ds.sample.
#' For further details see help for ds.sample and native R help for sample().
#' @param replace.transmit a Boolean indicator (TRUE or FALSE) specifying whether the
#' sample should be drawn with or without replacement. Default is FALSE so
#' the sample is drawn without replacement. replace.transmit is
#' fully specified by the [replace] argument of ds.sample. For further details see 
#' help for ds.sample and native R help for sample().
#' @param prob.transmit a character string containing the name of a numeric vector
#' of probability weights on the serverside that is associated with each of the
#' elements of the vector to be sampled enabling the drawing of a sample
#' with some elements given higher probability of being drawn than others.
#' prob.transmit is fully specified by the [prob] argument of ds.sample. For further details see 
#' help for ds.sample and native R help for sample().
#' @return the object specified by the <newobj> argument (or default name
#' 'newobj.sample') which is written to the serverside. For further details see 
#' help for ds.sample and native R help for sample().
#' @author Paul Burton, for DataSHIELD Development Team, 15/4/2020
#' @export
sampleDS <- function(x.transmit, size.transmit, replace.transmit=NULL, prob.transmit=NULL){
  
  #########################################################################
  # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
  thr<-listDisclosureSettingsDS()                                         #
  #nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
  #nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
  nfilter.subset<-as.numeric(thr$nfilter.subset)                          #
  #nfilter.string<-as.numeric(thr$nfilter.string)                         #
  nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)                #
  #nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
  #datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
  #########################################################################
  

  #Check character string denoting <x> argument is not potentially disclosive because of length 
  
  string.safe<-TRUE
  
  if(is.character(x.transmit))
  {
  	x.text<-strsplit(x.transmit, split="")
  	string.2.test<-unlist(x.text)
  	if(length(string.2.test)>nfilter.stringShort) string.safe<-FALSE
  }
  
  if(!string.safe)
  {
     studysideMessage<-"FAILED: the character string denoting the argument <x> is too long and may be disclosive - please shorten"
     return(list(studysideMessage=studysideMessage))
  }
  
  #Check character string denoting <prob> argument is not potentially disclosive because of length 
  string.safe<-TRUE
  
  if(is.character(prob.transmit))
  {
  	prob.text<-strsplit(prob.transmit, split="")
  	string.2.test<-unlist(prob.text)
  	if(length(string.2.test)>nfilter.stringShort) string.safe<-FALSE
  }
  
  if(!string.safe)
  {
     studysideMessage<-"FAILED: the character string denoting the argument <prob> is too long and may be disclosive - please shorten"
     return(list(studysideMessage=studysideMessage))
  }
  
  #Activate <x> and <prob> if they are character strings
  if(is.character(x.transmit))
  {
      x.active<-eval(parse(text=x.transmit), envir = parent.frame())
  
  	if(is.data.frame(x.active)||is.matrix(x.active))
  		{
  		object.length<-dim(x.active)[1]
  		colnames.x.active<-colnames(x.active)
  		}
  
  	if(!is.data.frame(x.active)||!is.matrix(x.active))
  		{
  		x.active<-data.frame(x.active)
  		object.length<-dim(x.active)[1]
  		if(dim(x.active)[2]==1)
  			{
  			colnames.x.active<-x.transmit
  			}
  		}
  
  #If size is 0 convert to length of object to enable random permutation if size=0
  
  	if(size.transmit==0)
  	{
  		size.transmit<-object.length
  	}
  
  #Check size <= length(x.active) if replace.transmit==FALSE
  if(is.character(x.transmit) && !replace.transmit)
  {
  	if(size.transmit>object.length)
  	{
  	studysideMessage<-"FAILED: if sampling without replacement size must be less than or equal to length(x)"
  	return(list(studysideMessage=studysideMessage))
  	}
  }
  
  if(is.numeric(x.transmit))
  {
  	if(size.transmit>x.active)
  	{
  	studysideMessage<-"FAILED: if sampling without replacement size must be less than or equal to x"
  	return(list(studysideMessage=studysideMessage))
  	}
  
  }
  
  #Check size of ultimate subset is not disclosive
  subset.length<-size.transmit
  complementary.subset.length<-object.length-size.transmit
  if(subset.length<nfilter.subset && subset.length!=0 && subset.length!=object.length)
  	{
  	studysideMessage<-"FAILED: disclosure risk, as the length of the subset to be created is less than nfilter.subset"
  	return(list(studysideMessage=studysideMessage))
  	}
  	
  if(complementary.subset.length<nfilter.subset&& subset.length!=0 && subset.length!=object.length)
  	{
  	studysideMessage<-"FAILED: disclosure risk using differencing: original object length minus subset length less than nfilter.subset"
  	return(list(studysideMessage=studysideMessage))
  
  	}
  }
  
  if(is.numeric(x.transmit))
  {
  x.active<-x.transmit
  }
  
  prob.active<-NULL
  if(is.character(prob.transmit))
  {
      prob.active<-eval(parse(text=prob.transmit), envir = parent.frame())
  }
  
  #Check size <= length(x.active) if replace.transmit==FALSE
  if(is.character(x.transmit) && !replace.transmit)
  {
  	if(size.transmit>object.length)
  	{
  	studysideMessage<-"FAILED: if sampling without replacement size must be less than or equal to length(x)"
  	return(list(studysideMessage=studysideMessage))
  	}
  }
  
  if(is.numeric(x.transmit))
  {
  	if(size.transmit>x.active)
  	{
  	studysideMessage<-"FAILED: if sampling without replacement size must be less than or equal to x"
  	return(list(studysideMessage=studysideMessage))
  	}
  
  }
  
  #If x.active is a vector, matrix or dataframe append marker vectors to its side
  
  if(is.character(x.transmit))
  {
  in.sample.vector<-rep(0,length=object.length)
  in.sample.order.vector<-rep(0,length=size.transmit)
  
  
  seq.id<-1:object.length
  
  object.2.subset<-data.frame(x.active,in.sample.vector,seq.id)
  
  num.cols<-dim(object.2.subset)[2]
  
  colnames(object.2.subset)<-c(colnames.x.active,"in.sample","ID.seq")
  
  #if size
  	if(size.transmit==0)
  	{
  		size.transmit<-object.length
  	}
  
  in.sample.ids<-sample(x=seq.id,size=size.transmit,replace=replace.transmit,prob.active)
  
  	for(f in 1:size.transmit)
  	{
  	object.2.subset[in.sample.ids[f],(num.cols-1)]<-1
  	in.sample.vector[in.sample.ids[f]]<-1
  	}
  	into.sample.order.vector<-order(in.sample.ids)
  
  object.2.subset.marked.up<-object.2.subset
  object.in.sample<-object.2.subset.marked.up[in.sample.vector==1,]
  object.non.sample<-object.2.subset.marked.up[in.sample.vector==0,]
  object.in.sample.marked.up<-data.frame(object.in.sample,into.sample.order.vector)
  
  colnames(object.in.sample.marked.up)<-c(colnames(object.2.subset),"sampling.order")
  
  outobj<-object.in.sample.marked.up
  }
  
  if(is.numeric(x.transmit))
  {
	if(size.transmit<=0)
		{
		outobj<-sample(x=x.active)
  		}else{
  		outobj<-sample(x=x.active,size=size.transmit,replace=replace.transmit,prob.active)
  		}
  }
  
  # Detects which column names (if any) have the '$' in their string and detach 
  # the '$' sign and any characters before that
  colnames.act1 <- colnames(outobj)
  detect.idx <- grep('[$]', colnames.act1)
  if(length(detect.idx) > 0){
    detach.names <- strsplit(colnames.act1[detect.idx], "\\$", perl=TRUE)
    for(i in 1:length(detach.names)){
      detach.names[i] <- detach.names[[i]][2]
    }
    colnames.act1[detect.idx] <- detach.names
  }
  colnames(outobj) <- colnames.act1
  
  
  return(outobj)

}
#assign function
# sampleDS
