#' @title Secure ranking of "V2BR" (vector to be ranked) across all sources
#' and use of these ranks to estimate global quantiles across all studies
#' @description identify the global values of V2BR (i.e. the values across all
#' studies) that relate to a set of quantiles to be evaluated.
#' @details Severside aggregate function called by ds.extractQuantiles via
#' ds.ranksSecure. This takes the "global.bounds.df" data frame saved on the
#' serverside following construction by extractQuantilesDS1. This
#' data frame includes the two quantile values that most closely span each
#' quartile value to be estimated. If either of the values had been the correct 
#' value for a given quantile, both the bounding values would have taken that
#' value in global.bounds.df. This is because the upper bound was defined as
#' the lowest value that was equal to or greater than the true value for that
#' quantile while the lower bound was defined as the highest value that was
#' equal to or lower than the true value. Next, the function extractQuantileDS2
#' goes round study by study to identify the values of V2BR that actually
#' correspond to each of the spanning values around each quantile. Then the
#' function goes quantile by quantile and estimates the mean of the
#' two values of V2BR that correspond to the the spanning quantiles. If these
#' two values are the same it means that that value of V2BR is the
#' "true" value and the mean of two (or potentially several) instances of that
#' value is inevitably also equal to that true value. If the upper and lower
#' bounding values of V2BR differ, neither can  be the precisely correct single
#' value of V2BR for that quantile (see above for explanation) and so the mean
#' of the two is a reasonable interpolated summary.
#' @param extract.summary.output.ranks.df character string specifies an optional
#' name for the data.frame written to the serverside on each data source that
#' contains 5 of the key output variables from the ranking procedure pertaining
#' to that particular data source. This data frame represents the key source of
#' information - including global ranks - that determines the values of V2BR
#' that are identified as corresponding to the particular set of quantiles to
#' be estimated as specified by the <quantiles.for.estimation> argument of
#' function ds.ranksSecure (and the <extract.quantiles> argument of
#' ds.extractQuantiles).
#' @return the single value of V2BR which best corresponds to each key quantile
#' value to be estimated as specified by the argument <quantiles.for.estimation>
#' A data frame (final.quantile.df)summarising the results of this analysis
#' is written to the clientside.  This data frame consists of two vectors. The
#' first is named "evaluation.quantiles". It lists the full set of quantiles
#' you have requested for evaluation as specified by the argument
#' "quantiles.for.estimation" The second vector which is called
#' "final.quantile.vector" details the values of V2BR that correspond to the
#' the key quantiles listed in vector 1.
#' @author Paul Burton 11th November, 2021
#' @export
extractQuantilesDS2 <- function(extract.summary.output.ranks.df){ #START FUNC

  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  thr<-dsBase::listDisclosureSettingsDS()                     #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  #nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################

extract.summary.ranks.df2 <- eval(parse(text=extract.summary.output.ranks.df), envir = parent.frame())
numsubs<-nrow(extract.summary.ranks.df2)
numvals<-length(global.bounds.df$evaluation.quantiles)


relevant.study.specific.input.values.lower<-rep(NA,numvals)
relevant.study.specific.input.values.upper<-rep(NA,numvals)

for(ww in 1:numvals){
  if(sum(extract.summary.ranks.df2$final.quantiles.global==global.bounds.df$lower.bound.vector[ww])>0)
  {
    relevant.study.specific.input.values.lower[ww]<-
    mean(extract.summary.ranks.df2$input.var.real.orig[extract.summary.ranks.df2$final.quantiles.global
                                        ==global.bounds.df$lower.bound.vector[ww]])
  }

  if(sum(extract.summary.ranks.df2$final.quantiles.global==global.bounds.df$upper.bound.vector[ww])>0)
  {
  relevant.study.specific.input.values.upper[ww]<-
    mean(extract.summary.ranks.df2$input.var.real.orig[extract.summary.ranks.df2$final.quantiles.global
                                        ==global.bounds.df$upper.bound.vector[ww]])
  }
}


    return(list(relevant.study.specific.input.values.lower=relevant.study.specific.input.values.lower,
                relevant.study.specific.input.values.upper=relevant.study.specific.input.values.upper))
}  

#AGGREGATE
# extractQuantilesDS2

  


