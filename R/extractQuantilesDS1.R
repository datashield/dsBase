#' @title Secure ranking of "V2BR" (vector to be ranked) across all sources
#' and use of these ranks to estimate global quantiles across all studies
#' @description identify the global values of V2BR (i.e. the values across all
#' studies) that relate to a set of quantiles to be evaluated.
#' @details Severside aggregate function called by ds.extractQuantiles via
#' ds.ranksSecure. As well as estimating the key values of V2BR that correspond
#' to the selected quantiles, this function also implements a disclosure control
#' trap. If the ratio of the total number of all observations across all studies
#' divided by the number of quantile values to be estimated is less than or
#' equal to nfilter.subset (which specifies the minimum size of a subset) the
#' process stops and an error message is returned suggesting that you might
#' try selecting a narrower range of quantiles with less quantile values to be
#' estimated as specified by the argument <quantiles.for.estimation> of the
#' function ds.ranksSecure. For more details about the cluster of functions that
#' collectively enable secure global ranking and estimation of global quantiles
#' see the associated document entitled "secure.global.ranking.docx"
#' @param extract.quantiles one of a restricted set of character strings that
#' fix the set of quantile values for which the corresponding values across
#' all studies are to be estimated. For more details
#' see the associated document entitled "secure.global.ranking.docx", the
#' header for ds.ranksSecure and ds.extractQuantiles functions.
#' The value of this argument is set in choosing the value of the argument
#' <quantiles.for.estimation> in ds.ranksSecure.
#' @param extract.summary.output.ranks.df character string specifying optional
#' name for the data.frame written to the serverside on each data source that
#' contains 5 of the key output variables from the ranking procedure pertaining
#' to that particular data source. This data frame represents the key source of
#' information - including global ranks - that determines the values of V2BR
#' that are identified as corresponding to the particular set of quantiles to
#' be estimated as specified by the <quantiles.for.estimation> argument of
#' function ds.ranksSecure (and the <extract.quantiles> argument of
#' ds.extractQuantiles).
#' @return as a first step in creating the vector of values of values of V2BR
#' that correspond to each quantile value, extractQuantilesDS1 identifies the
#' two closest quantile values across all studies that span each key quantile
#' value. These are saved as the data frame "closest.bounds.df" on the
#' clientside and then saved on the serverside by ds.dmtC2S into the data frame
#' "global.bounds.df". Also if the number of observations across all studies is
#' too small, and a  disclosure risk exists if the final.quantile.vector is made
#' available via the client, this function stops the processing and returns a
#' warning/error message.  
#' @author Paul Burton 11th November, 2021
#' @export
extractQuantilesDS1 <- function(extract.quantiles,extract.summary.output.ranks.df){ #START FUNC

  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  thr<-dsBase::listDisclosureSettingsDS()                     #
  nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  #nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################
 
  #############################################   
 
  extract.summary.ranks.df <- eval(parse(text=extract.summary.output.ranks.df), envir = parent.frame())
  
 #Specify quantiles to evaluate
  if(extract.quantiles=="0.025-0.975"){
  evaluation.quantiles<-c(0.025,0.05,0.10,0.20,0.25,0.30,0.3333,0.40,0.50,0.60,0.6667,0.70,0.75,0.80,0.90,0.95,0.975)
  }
  if(extract.quantiles=="0.05-0.95"){
    evaluation.quantiles<-c(0.05,0.10,0.20,0.25,0.30,0.3333,0.40,0.50,0.60,0.6667,0.70,0.75,0.80,0.90,0.95)
  }
  if(extract.quantiles=="0.10-0.90"){
    evaluation.quantiles<-c(0.10,0.20,0.25,0.30,0.3333,0.40,0.50,0.60,0.6667,0.70,0.75,0.80,0.90)
  }
  if(extract.quantiles=="0.20-0.80"){
    evaluation.quantiles<-c(0.20,0.25,0.30,0.3333,0.40,0.50,0.60,0.6667,0.70,0.75,0.80)
  }
  if(extract.quantiles=="quartiles"){
    evaluation.quantiles<-c(0.25,0.50,0.75)
  }
  if(extract.quantiles=="median"){
    evaluation.quantiles<-c(0.50)
  }
  
  
  numvals<-length(evaluation.quantiles)
  numsubs.in.study<-nrow(extract.summary.ranks.df)
  
  #calculate total number of real data observations in variable to be ranked
  #across all studies to prime disclosure control

  #Identify specific observations defining chosen evaluation quantiles
  quants<-extract.summary.ranks.df$final.quantiles.global
   ranks<-extract.summary.ranks.df$final.ranks.global
   numsubs.real<-max(ranks/quants)
   
   if((numsubs.real/numvals)<=nfilter.tab){
     error.message<-
       paste0("FAILED: the total number of observations across all studies is so small that there is a disclosure risk in releasing the list of quantiles requested. You could change the quantiles.for.estimation argument to request a narrower range of quantiles to be be estimated.")
     stop(error.message, call. = FALSE)
   }
   
  
  lower.bound<-rep(NA,numvals)
  upper.bound<-rep(NA,numvals)
  
  num.real.all.studies<-max()
  for(qq in 1:numvals){
    disclosure.risk<-FALSE
        if(length(quants<=evaluation.quantiles[qq])<nfilter.subset){
          disclosure.risk<-TRUE
        }  
 
        if(length(quants>=evaluation.quantiles[qq])<nfilter.subset){
        disclosure.risk<-TRUE
        }  
    
    lower.bound[qq]<-max(quants[quants<=evaluation.quantiles[qq]])
    upper.bound[qq]<-min(quants[quants>=evaluation.quantiles[qq]])

  }
  
bounds.df<-data.frame(evaluation.quantiles,lower.bound,upper.bound)

colnames(bounds.df)<-c("evaluation.quantiles","lower.bound","upper.bound")

return(bounds.df)
}

#AGGREGATE
# extractQuantilesDS1

  


