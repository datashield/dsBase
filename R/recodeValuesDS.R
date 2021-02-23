#'
#' @title recodeValuesDS an assign function called by ds.recodeValues
#' @description This function recodes specified values of elements in a vector into
#' a matched set of alternative specified values.
#' @details For all details see the help header for ds.recodeValues
#' @param var.name.text a character string providing the name for the vector representing the
#' variable to be recoded. <var.name.text> argument generated and passed directly to
#' recodeValuesDS by ds.recodeValues
#' @param values2replace.text a character string specifying the values in the
#' vector specified by the argument <var.name.text> that are to be replaced by new
#' values as specified in the new.values.vector. The <values2replace.text> argument
#' is generated and passed directly to recodeValuesDS by ds.recodeValues. In effect, the
#' <values2replace.vector> argument of the ds.recodeValues function is converted
#' to a character string format that is acceptable to the DataSHIELD R parser in the data repository
#' and so can be accepted by recodeValuesDS
#' @param new.values.text a character string specifying the new values to which
#' the specified values in the vector <var.name> are to be converted.
#' The <new.values.text> argument is generated and passed directly to recodeValuesDS
#' by ds.recodeValues. In effect, the <new.values.vector> argument of the
#' ds.recodeValues function is converted to a character string format that is
#' acceptable to the DataSHIELD R parser in the data repository
#' and so can be used in the call to recodeValuesDS.
#' @param missing if supplied, any missing values in the variable referred to by var.name.text 
#' will be replaced by this value.
#' @return the object specified by the <newobj> argument (or default name '<var.name>_recoded')
#' initially specified in calling ds.recodeValues. The output object (the required
#' recoded variable called <newobj> is written to the serverside.
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
recodeValuesDS <- function(var.name.text=NULL, values2replace.text=NULL, new.values.text=NULL, missing=NULL){

  #############################################################
  #MODULE 1: CAPTURE THE used nfilter SETTINGS
  thr <- listDisclosureSettingsDS()	
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)
  #############################################################
  
  # DISCLOSURE TRAPS
  var.name.text.chars <- strsplit(var.name.text, split="")
  if(length(var.name.text.chars[[1]]) > nfilter.stringShort){
     studysideMessage <- "Error: var.name.text argument too long (see nfilter.stringShort)"
     stop(studysideMessage, call. = FALSE)
  }
  
  values2replace.text.chars <- strsplit(values2replace.text, split="")
  if(length(values2replace.text.chars[[1]]) > nfilter.stringShort){
    studysideMessage <- "Error: values2replace.text argument too long (see nfilter.stringShort)"
    stop(studysideMessage, call. = FALSE)
  }
  
  new.values.text.chars <- strsplit(new.values.text, split="")
  if(length(new.values.text.chars[[1]]) > nfilter.stringShort){
    studysideMessage <- "Error: new.values.text argument too long (see nfilter.stringShort)"
    stop(studysideMessage, call. = FALSE)
  }

  
  var2recode <- eval(parse(text=var.name.text), envir = parent.frame())
  
  values2replace <- unlist(strsplit(values2replace.text, split=","))
  new.values <- unlist(strsplit(new.values.text, split=","))
  
  if(!is.null(missing)){missing <- as.numeric(missing)}
  
  # get the class of the input variable
  var.class <- class(var2recode)
  
  # if the class of the input variable is not factor, numeric, character or integer then 
  # stop and return an error message
  if (!(var.class %in% c('factor', 'character', 'numeric', 'integer'))){
    studysideMessage <- "Error: The variable to recode must be either a factor, a character, a numeric or an integer"
    stop(studysideMessage, call. = FALSE)
  }
  
  # recode using the recode function from the dplyr package 
  if (var.class == 'factor'){
    expr <- as.list(new.values)
    names(expr) <- values2replace
    var.recoded <- dplyr::recode_factor(var2recode, !!!(expr))
    if (!is.null(missing)){
      var.recoded.tmp <- var.recoded
      var.recoded <- addNA(var.recoded.tmp)
      levels(var.recoded) <- c(levels(var.recoded.tmp), missing)
    }
  }
  if (var.class == 'character'){
    expr <- as.list(new.values)
    names(expr) <- values2replace
    var.recoded <- dplyr::recode(var2recode, !!!(expr), .missing=paste0("'", missing, "'"))
  }
  if (var.class == 'numeric'){
    expr <- as.list(as.numeric(new.values))
    names(expr) <- values2replace
    var.recoded <- dplyr::recode(var2recode, !!!(expr), .missing=missing)
  }
  if (var.class == 'integer'){
    expr <- as.list(as.numeric(new.values))
    names(expr) <- values2replace
    var2recode.n <- as.numeric(var2recode)
    var.recoded <- dplyr::recode(var2recode.n, !!!(expr), .missing=missing)
    var.recoded <- as.integer(var.recoded)
  }

  # DISCLOSURE TRAP ON LENGTH OF NA AND non-NA ELEMENTS OF ORIGINAL AND RECODED VECTORS
  mark.original <- stats::complete.cases(var2recode)
  non.NA.original.vector <- var2recode[mark.original]
  non.NA.length.original <- length(non.NA.original.vector)
  
  mark.recoded <- stats::complete.cases(var.recoded)
  non.NA.recoded.vector <- var.recoded[mark.recoded]
  non.NA.length.recoded <- length(non.NA.recoded.vector)
  
  difference.non.NA.lengths <- abs(non.NA.length.recoded-non.NA.length.original)
  
  # Non-NA SUBSET OF RECODED VARIABLE SMALLER THAN MINIMUM SUBSET SIZE - BLOCK CREATION OF RECODED VECTOR
  # AND RETURN MESSAGE
  if(non.NA.length.recoded < nfilter.subset){
     studysideMessage <- "Error: number of non-NA elements of recoded vector < minimum subset size"
     stop(studysideMessage, call. = FALSE)
  }
  
  ########################################################################
  ##########MODULE WARNING OF POTENTIAL DIFFERENCE ATTACK ################
  ########################################################################
  
  if((difference.non.NA.lengths < nfilter.subset) && (difference.non.NA.lengths > 0)){
    studysideWarning1 <- "Warning: DataSHIELD monitors every session for potentially disclosive analytic requests."
    studysideWarning2 <- "The analysis you just submitted has generated a recoded variable in which the number of non-missing"
    studysideWarning3 <- "elements differs - but only very slightly - from the original variable. This is most likely to be"
    studysideWarning4 <- "an innocent consequence of your recoding needs. However, it could in theory be one step"
    studysideWarning5 <- "in a difference-based attack aimed at identifying individuals. This analytic request has"
    studysideWarning6 <- "therefore been highlighted in the session log file. Please be reassured, if you do not try"
    studysideWarning7 <- "to identify individuals this will cause you no difficulty. However, if you do plan a "
    studysideWarning8 <- "malicious attempt to identify individuals by differencing, this will become obvious in the"
    studysideWarning9 <- "session log and you will be sanctioned. Possible consequences include loss of future access"
    studysideWarning10 <- "to DataSHIELD and/or legal penalties."
    return.message <- list(studysideWarning1, studysideWarning2, studysideWarning3, studysideWarning4,
                           studysideWarning5, studysideWarning6, studysideWarning7, studysideWarning8,
                           studysideWarning9, studysideWarning10)
    warning(return.message, call. = FALSE)
  }
  
  return(var.recoded)

}
# ASSIGN FUNCTION
# recodeValuesDS
