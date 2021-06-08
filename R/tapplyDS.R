#' @title tapplyDS called by ds.tapply
#' @description Apply one of a selected range of functions to summarize an
#' outcome variable over one or more indexing factors and write the resultant
#' summary to the clientside
#' @details see details for {ds.tapply} function
#' @param X.name, the name of the variable to be summarized. Specified
#' via argument <X.name> of {ds.tapply} function
#' @param INDEX.names.transmit, the name of a single factor or a vector of names of factors to
#' index the variable to be summarized. Specified via argument <INDEX.names>
#' of {ds.tapply} function
#' @param FUN.name, the name of one of the allowable summarizing functions to be applied.
#' Specified via argument <FUN.name> of {ds.tapply} function.
#' @return an array of the summarized values created by the tapplyDS function. This array
#' is returned to the clientside. It has the same number of dimensions as INDEX.
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
tapplyDS <- function(X.name, INDEX.names.transmit, FUN.name){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS           			#
thr<-listDisclosureSettingsDS()							#
nfilter.tab<-as.numeric(thr$nfilter.tab)								#
#nfilter.glm<-as.numeric(thr$nfilter.glm)								#
nfilter.subset<-as.numeric(thr$nfilter.subset)          				#
#nfilter.string<-as.numeric(thr$nfilter.string)              			#
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)    			#
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)								#
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)        #
#########################################################################

  if(is.character(X.name)){
	  X<-eval(parse(text=X.name), envir = parent.frame())
	}else{
    studysideMessage<-"ERROR: X.name must be specified as a character string"
    stop(studysideMessage, call. = FALSE)
  }

INDEX.factors<-unlist(strsplit(INDEX.names.transmit, split=","))

num.factors<-length(INDEX.factors)

#test that output vector and indexing factor all have the same length
length.2.test<-length(X)

length.test.vector<-rep(NA,num.factors)

for(g in 1:num.factors){
activation.text.0<-paste0("INDEX.factors[",g,"]")
active.factor.name<-eval(parse(text=activation.text.0))
active.factor<-eval(parse(text=active.factor.name), envir = parent.frame())
active.factor
length.test.vector[g]<-length(active.factor)
}

all.lengths.equal<-TRUE
for(h in 1:num.factors){

if(length.2.test!=length.test.vector[h])all.lengths.equal<-FALSE
}

if(!all.lengths.equal){
return.message="Error: the output variable and all indexing factors must be of equal length"
stop(return.message, call. = FALSE)
}



#select complete cases on X and all INDEX factors only
all.complete<-stats::complete.cases(X)

current.factor <- NA
for(j in 1:num.factors){
  
activation.text.a<-paste0(INDEX.factors[j])
current.factor <- eval(parse(text=activation.text.a), envir = parent.frame())

all.complete<-all.complete&stats::complete.cases(current.factor)
}

X.complete<-X[all.complete]

for(k in 1:num.factors){

  activation.text.b<-paste0(INDEX.factors[k])
  current.factor <- eval(parse(text=activation.text.b), envir = parent.frame())
  
  activation.text.c<-paste0(INDEX.factors[k], "<- current.factor[all.complete]")
  eval(parse(text=activation.text.c))
 }

#Outcome vector and index factors now all reduced to complete cases only

#convert INDEX.names format from transmittable to actionable form (a list of vectors)
   INDEX.names.list<-paste0("list(",INDEX.names.transmit,")")
   INDEX<-eval(parse(text=INDEX.names.list))

 ##################
 #disclosure traps#
 ##################
   N.count <- tapply(X.complete,INDEX,base::length)

   if(min(N.count)<nfilter.tab && min(N.count) > 0){
   return.message<-"ERROR: at least one group defined by INDEX has < nfilter.tab members. The output cannot therefore be returned to the clientside. But the function ds.tapply.assign may still be used to write the output to the data servers with no clientside return"
   stop(return.message, call. = FALSE)
   }

 ##################
 ###other errors###
 ##################


  #################
  #Valid functions#
  #################

  ###MEAN
   if(FUN.name=="mean" || FUN.name=="Mean" || FUN.name=="MEAN"){

   Mean <- tapply(X.complete,INDEX,base::mean)
   N.count <- tapply(X.complete,INDEX,base::length)

   #make output neat if up to two INDEX factors
		if(num.factors==1){

                factor1.levels <- NA
		activation.text.e<-paste0("factor1.levels<-levels(",INDEX.factors[1],")")
		eval(parse(text=activation.text.e))

		factor1.level.names<-factor1.levels

		for(u in 1:length(factor1.levels)){
		factor1.level.names[u]<-paste0(INDEX.factors[1],".",factor1.levels[u])
		}
		dimnames(Mean)[[1]]<-factor1.level.names
		dimnames(N.count)[[1]]<-factor1.level.names
		}

		if(num.factors==2){

                factor1.levels <- NA
		activation.text.f<-paste0("factor1.levels<-levels(",INDEX.factors[1],")")
		eval(parse(text=activation.text.f))

                factor2.levels <- NA
		activation.text.g<-paste0("factor2.levels<-levels(",INDEX.factors[2],")")
		eval(parse(text=activation.text.g))

		factor1.level.names<-factor1.levels

		factor2.level.names<-factor2.levels

		for(u in 1:length(factor1.levels)){
		factor1.level.names[u]<-paste0(INDEX.factors[1],".",factor1.levels[u])
		}

		for(v in 1:length(factor2.levels)){
		factor2.level.names[v]<-paste0(INDEX.factors[2],".",factor2.levels[v])
		}


		dimnames(Mean)[[1]]<-factor1.level.names
		dimnames(Mean)[[2]]<-factor2.level.names

		dimnames(N.count)[[1]]<-factor1.level.names
		dimnames(N.count)[[2]]<-factor2.level.names
		}


 output<-list(Mean=Mean,N=N.count)
 return(output)
 }

   ###SD
   if(FUN.name=="sd" || FUN.name=="SD"){

   SD <- tapply(X.complete,INDEX,stats::sd)
   N.count <- tapply(X.complete,INDEX,base::length)

   #make output neat if up to two INDEX factors
		if(num.factors==1){

                factor1.levels <- NA
		activation.text.e<-paste0("factor1.levels<-levels(",INDEX.factors[1],")")
		eval(parse(text=activation.text.e))

		factor1.level.names<-factor1.levels

		for(u in 1:length(factor1.levels)){
		factor1.level.names[u]<-paste0(INDEX.factors[1],".",factor1.levels[u])
		}
		dimnames(SD)[[1]]<-factor1.level.names
		dimnames(N.count)[[1]]<-factor1.level.names
		}

		if(num.factors==2){

                factor1.levels <- NA
		activation.text.f<-paste0("factor1.levels<-levels(",INDEX.factors[1],")")
		eval(parse(text=activation.text.f))

                factor2.levels <- NA
		activation.text.g<-paste0("factor2.levels<-levels(",INDEX.factors[2],")")
		eval(parse(text=activation.text.g))

		factor1.level.names<-factor1.levels

		factor2.level.names<-factor2.levels

		for(u in 1:length(factor1.levels)){
		factor1.level.names[u]<-paste0(INDEX.factors[1],".",factor1.levels[u])
		}

		for(v in 1:length(factor2.levels)){
		factor2.level.names[v]<-paste0(INDEX.factors[2],".",factor2.levels[v])
		}


		dimnames(SD)[[1]]<-factor1.level.names
		dimnames(SD)[[2]]<-factor2.level.names

		dimnames(N.count)[[1]]<-factor1.level.names
		dimnames(N.count)[[2]]<-factor2.level.names
		}


 output<-list(SD=SD,N=N.count)
 return(output)
 }


   ###SUM
  if(FUN.name=="sum" || FUN.name=="Sum" || FUN.name=="SUM"){
   Sum <- tapply(X.complete,INDEX,base::sum)
   N.count <- tapply(X.complete,INDEX,base::length)

   #make output neat if up to two INDEX factors
		if(num.factors==1){

                factor1.levels <- NA
		activation.text.e<-paste0("factor1.levels<-levels(",INDEX.factors[1],")")
		eval(parse(text=activation.text.e))

		factor1.level.names<-factor1.levels

		for(u in 1:length(factor1.levels)){
		factor1.level.names[u]<-paste0(INDEX.factors[1],".",factor1.levels[u])
		}
		dimnames(Sum)[[1]]<-factor1.level.names
		dimnames(N.count)[[1]]<-factor1.level.names
		}

		if(num.factors==2){

                factor1.levels <- NA
		activation.text.f<-paste0("factor1.levels<-levels(",INDEX.factors[1],")")
		eval(parse(text=activation.text.f))

                factor2.levels <- NA
		activation.text.g<-paste0("factor2.levels<-levels(",INDEX.factors[2],")")
		eval(parse(text=activation.text.g))

		factor1.level.names<-factor1.levels

		factor2.level.names<-factor2.levels

		for(u in 1:length(factor1.levels)){
		factor1.level.names[u]<-paste0(INDEX.factors[1],".",factor1.levels[u])
		}

		for(v in 1:length(factor2.levels)){
		factor2.level.names[v]<-paste0(INDEX.factors[2],".",factor2.levels[v])
		}


		dimnames(Sum)[[1]]<-factor1.level.names
		dimnames(Sum)[[2]]<-factor2.level.names

		dimnames(N.count)[[1]]<-factor1.level.names
		dimnames(N.count)[[2]]<-factor2.level.names
		}


 output<-list(Sum=Sum,N=N.count)
 return(output)
 }


    ###QUANTILE
  if(FUN.name=="quantile" || FUN.name=="Quantile" || FUN.name=="QUANTILE"){

	if(num.factors>1){
	return.message<-"Quantile will only work with one indexing factor but you can combine several factors into one. e.g. two factors with f1 and f2 levels respectively can be combined into one with f1 x f2 levels"
	return(return.message)
	}
  probs.vector <- c(0.05,0.1,0.2,0.25,0.3,0.33,0.4,0.5,0.6,0.67,0.7,0.75,0.8,0.9,0.95)
  Quantile <- tapply(X.complete,INDEX,stats::quantile, probs=probs.vector)
  N.count <- tapply(X.complete,INDEX,base::length)


 #output<-list(Quantile=Quantile,N=N.count)
 return(Quantile)
 }



 return.message<-"Please specify a valid DataSHIELD tapply function e.g. FUN.name='mean' or FUN.name='sd'"
 return(return.message)
}
#AGGREGATE.FUNCTION
# tapplyDS
