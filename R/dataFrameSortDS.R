#'
#' @title Sorting and reordering data frames, vectors or matrices
#' @description Sorts a data frame using a specified alphanumeric or numeric sort key
#' @details Serverside assign function dataFrameSortDS is called by
#' clientside function ds.dataFrameSort.  A vector or a matrix can be
#' added to, or coerced into, a data frame (using function [ds.dataFrame])
#' and this means that they too can be sorted/reordered using ds.dataFrameSort.
#' Fundamentally, the function [ds.dataFrameSort] will sort a specified
#' data frame on the serverside using a sort key also on the serverside.
#' For more details see help for the clientside function: [ds.dataFrameShort]
#' @param df.name a character string providing the name for the serverside
#' data.frame to be sorted. This parameter is fully specified by the equivalent
#' argument in ds.dataFrameShort and further details can be found at
#' help("ds.dataFrameSort").
#' @param sort.key.name a character string providing the name for the sort key.
#' This will be a serverside vector which may sit inside the data frame to be
#' sorted or independently in the serverside analysis environment. But, if it
#' sits outside the data frame it must then be the same length as the data frame.
#' This parameter is fully specified by the equivalent argument
#' in ds.dataFrameShort and further details can be found at help("ds.dataFrameSort").
#' @param sort.descending logical, if TRUE the data.frame will be sorted
#' by the sort key in descending order. Default = FALSE (sort order ascending).
#' This parameter is fully specified by the equivalent argument
#' in ds.dataFrameShort and further details can be found at help("ds.dataFrameSort").
#' @param sort.method A character string taking one of the values: "default",
#' "d", "alphabetic", "a", "numeric", "n", or NULL. Default value is "default".
#' This parameter is fully specified by the equivalent argument
#' in ds.dataFrameShort and further details can be found at help("ds.dataFrameSort").
#' @return the appropriately re-sorted data.frame will be written to the serverside
#' R environment as a data.frame named according to the <newobj> argument(or with
#' default name 'dataframesort.newobj') if no name is specified
#' @author Paul Burton, with critical error identification by
#' Leire Abarrategui-Martinez, for DataSHIELD Development Team, 2/4/2020
#' @export
#'
dataFrameSortDS <- function(df.name=NULL,sort.key.name=NULL,sort.descending,sort.method){

  #########################################################################
  # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  #nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  nfilter.string <- as.numeric(thr$nfilter.string)
  nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)
  #datashield.privacyLevel <- as.numeric(thr$datashield.privacyLevel)
  #########################################################################

  # DISCLOSURE TRAPS
  df.name.chars <- strsplit(df.name,split="")
  if(length(df.name.chars[[1]])>nfilter.stringShort){
    studysideMessage <- "df.name argument could hide active code - please use shorter name"
    return(list(studysideMessage=studysideMessage))
  }


 sort.key.name.chars <- strsplit(sort.key.name,split="")

  if(length(sort.key.name.chars[[1]])>nfilter.stringShort){
    studysideMessage <- "sort.key.name argument could hide active code - please use shorter name"
    return(list(studysideMessage=studysideMessage))
  }

  df.name.2 <- paste0("data.frame(",df.name,")")
  df2sort   <- eval(parse(text=df.name.2), envir = parent.frame())
  sort.key  <- eval(parse(text=sort.key.name), envir = parent.frame())

  # DISCLOSURE TRAPS
  if(dim(df2sort)[1]<nfilter.subset){
    studysideMessage <- "specified data.frame to sort is shorter than minimum subset size"
    return(list(studysideMessage=studysideMessage))
  }

  if(length(sort.key)<nfilter.subset){
    studysideMessage <- "specified sort.key variable is shorter than minimum subset size"
    return(list(studysideMessage=studysideMessage))
  }

  if((sort.method=="alphabetic") || ((sort.method=="default") && is.character(sort.key))){
     sort.key.2.use<-as.character(sort.key)
     key.order <- stringr::str_order(sort.key.2.use, decreasing = sort.descending, na_last = TRUE)
  }else{
#PUT na.last in here
     sort.key.2.use<-as.numeric(sort.key)

     if(sort.descending)
     {
        key.ranks <- rank(sort.key.2.use,ties.method="average",na.last=FALSE)
         key.ranks <-(length(key.ranks)+1)-key.ranks
     }else{
        key.ranks <- rank(sort.key.2.use,ties.method="average",na.last=TRUE)
     }
      key.order <- order(key.ranks)
      }
    df.sorted <- df2sort[key.order,]  
    return(df.sorted)
}
# ASSIGN FUNCTION
# dataFrameSortDS
