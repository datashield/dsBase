#' 
#' @title dataFrameSortDS.o called by ds.dataFrameSort.o
#' @description The serverside function that sorts a data frame using
#' a specified sort key.
#' @details A data frame is a list of variables all with the same number of rows,
#' which is of class 'data.frame'. For details of numeric and alphabetic
#' sorting and how ds.dataFrameSort.o/dataFrameSortDS.o jointly operate,
#' please see help for ds.dataFrameSort.o. 
#' @param df.text a character string providing the name for the data.frame
#' to be sorted. This corresponds to the argument <df.name> in ds.dataFrameSort.o
#' @param sort.key.text a character string providing the name for the sort key.
#' This corresponds to the argument <sort.key.name> in ds.dataFrameSort.o
#' @param sort.descending logical, if TRUE the data.frame will be sorted
#' by the sort key in descending order. Default = FALSE (sort order ascending)
#' @param sort.alphabetic logical, if TRUE the sort key is treated as if alphabetic
#' Default=FALSE.
#' @param sort.numeric logical, if TRUE the sort key is treated as if numeric
#' Default=FALSE. The arguments <sort.alphabetic> and <sort.numeric> are both
#' derived directly from the corresponding arguments specified in ds.dataFrameSort.o
#' If both sort.alphabetic and sort.numeric are FALSE, the sort.key will
#' interpreted naturally: as numeric if it is numeric, otherwise as alphabetic
#' ie as if it is a vector of character strings. 
#' @return the appropriately re-sorted data.frame will be written to the serverside
#' R environmnet as a data.frame named according to the <newobj> argument in
#' ds.dataFrameSortDS.o (or with default name <df.name>.sorted where <df.name> is the first argument of
#' ds.dataFrameSortDS.o)
#' @author DataSHIELD Development Team
#' @export
#'
dataFrameSortDS.o <- function(df.text=NULL,sort.key.text=NULL,sort.descending=FALSE,sort.alphabetic=FALSE,sort.numeric=FALSE){

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS
thr <- listDisclosureSettingsDS.o()
#nfilter.tab <- as.numeric(thr$nfilter.tab)
#nfilter.glm <- as.numeric(thr$nfilter.glm)	
nfilter.subset <- as.numeric(thr$nfilter.subset)
nfilter.string <- as.numeric(thr$nfilter.string)
nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)
#nfilter.kNN <- as.numeric(thr$nfilter.kNN)
#datashield.privacyLevel <- as.numeric(thr$datashield.privacyLevel)
#########################################################################

  # DISCLOSURE TRAPS
  df.text.chars <- strsplit(df.text,split="")
  if(length(df.text.chars[[1]])>nfilter.stringShort){
    studysideMessage <- "df.text argument could hide active code - please use shorter name"
    return(list(studysideMessage=studysideMessage))
  }

  sort.key.chars <- strsplit(sort.key.text,split="")

  if(length(sort.key.chars[[1]])>nfilter.stringShort){
    studysideMessage <- "sort.key.text argument could hide active code - please use shorter name"
    return(list(studysideMessage=studysideMessage))
  }

  df.text2 <- paste0("data.frame(",df.text,")")
  df2sort <- eval(parse(text=df.text2))

  sort.key <- eval(parse(text=sort.key.text))

  if(dim(df2sort)[1]<nfilter.subset){
    studysideMessage <- "specified data.frame to sort is shorter than minimum subset size"
    return(list(studysideMessage=studysideMessage))
  }

  if(length(sort.key)<nfilter.subset){
    studysideMessage <- "specified sort.key variable is shorter than minimum subset size"
    return(list(studysideMessage=studysideMessage))
  }
   
  if(sort.alphabetic){
    sort.key <- as.character(sort.key)
  }
  if(sort.numeric){
    sort.key <- as.numeric(sort.key)
  } 
   
  key.order <- order(sort.key)
   
  df.sorted <- df2sort[key.order,]

  if(sort.descending){
    key.rev.order <- length(key.order):1
    df.sorted <- df.sorted[key.rev.order,]
  }
   
  return(df.sorted)

}  
# ASSIGN FUNCTION
# dataFrameSortDS.o
