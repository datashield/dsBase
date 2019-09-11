#' @title reShapeDS (assign function) called by ds.reShape
#' @description Reshapes a data frame containing longitudinal or
#' otherwise grouped data from 'wide' to 'long' format or vice-versa
#' @details This function is based on the native R function \code{reshape}.
#' It reshapes a data frame containing longitudinal or otherwise grouped data
#' between 'wide' format with repeated
#' measurements in separate columns of the same record and 'long' format with the repeated
#' measurements in separate records. The reshaping can be in either direction
#' @param data.name, the name of the data.frame to be reshaped. Specified
#' via argument <data.name> of {ds.reShape} function
#' @param varying.transmit, names of sets of variables in the wide format that
#' correspond to single variables in long format (typically what may be called
#' 'time-varying' or 'time-dependent' variables). Specified
#' via argument <varying> of {ds.reShape} function.
#' @param v.names.transmit, the names of variables in the long format that correspond
#' to multiple variables
#' in the wide format - for example, sbp7, sbp11, sbp15 (measured systolic blood pressure
#' at ages 7, 11 and 15 years). Specified
#' via argument <v.names> of {ds.reShape} function
#' @param timevar.name, the variable in long format that differentiates multiple records
#' from the same group or individual. Specified
#' via argument <timevar.name> of {ds.reShape} function
#' @param idvar.name, names of one or more variables in long format that identify
#' multiple records from
#' the same group/individual. This/these variable(s) may also be present in wide format.
#' Specified via argument <idvar.name> of {ds.reShape} function
#' @param drop.transmit, a vector of names of variables to drop before reshaping. Specified
#' via argument <drop> of {ds.reShape} function
#' @param direction, a character string, partially matched to either "wide" to reshape from
#' long to wide format, or "long" to reshape from wide to long format. Specified
#' via argument <direction> of {ds.reShape} function
#' @param sep, a character vector of length 1, indicating a separating character in the variable
#' names in the wide format. Specified
#' via argument <sep> of {ds.reShape} function
#' @return a reshaped data.frame converted from long to wide format or from wide to
#' long format which is written to the serverside and given the name provided as the
#' <newobj> argument of {ds.reShape} or 'newObject' if no name is specified.
#' In addition, two validity messages are returned to the clientside
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form (see header for {ds.reShape}.
#' @author Demetris Avraam, Paul Burton for DataSHIELD Development Team
#' @export
reShapeDS <- function(data.name, varying.transmit, v.names.transmit, timevar.name, idvar.name, drop.transmit, direction, sep){

  datatext <- paste0("data.frame(",data.name,")")
  data <- eval(parse(text=datatext), envir = parent.frame())

  timevar <- timevar.name
  idvar <- idvar.name
  direction <- direction
  sep <- sep




  if(!is.null(varying.transmit)){
  varying<-unlist(strsplit(varying.transmit, split=","))
  }else{
  varying<-NULL
  }

    if(!is.null(v.names.transmit)){
  v.names<-unlist(strsplit(v.names.transmit, split=","))
  }else{
  v.names<-NULL
  }


  if(!is.null(drop.transmit)){
    drop<-unlist(strsplit(drop.transmit, split=","))
  }else{
    drop<-NULL
  }



  split = if (sep == "") {
    list(regexp = "[A-Za-z][0-9]", include = TRUE)
  } else {
    list(regexp = sep, include = FALSE, fixed = TRUE)
  }

 # ids<-1L:NROW(data)
 # times<-seq_along(varying)
 # times<-t(matrix(times))

 if(direction=="wide"){
 output <- stats::reshape(data=data, varying=varying, v.names=v.names, timevar=timevar,
                          idvar=idvar,
                          drop=drop, direction=direction, new.row.names = NULL, sep=sep, split=split)
 }

 if(direction=="long"){
    output <- stats::reshape(data=data, varying=varying, timevar=timevar,
                             idvar=idvar,
                             direction=direction)
 }


  return(output)


}
#Assign function
# reShapeDS
