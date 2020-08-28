#' @title cbindDS called by ds.cbind
#' @description serverside assign function that takes a
#' sequence of vector, matrix or data-frame arguments
#' and combines them by column to produce a data-frame.
#' @details A sequence of vector, matrix or data-frame arguments
#' is combined column by column to produce a data-frame
#' which is written to the server-side. A critical requirement is that
#' the length of all component variables, and the number of rows of the
#' component data.frames or matrices must all be the same. The output
#' data.frame will then have this same number of rows. For more details 
#' see help for \code{ds.cbind} and the native R function \code{cbind}.
#' @param x.names.transmit This is a vector of character strings
#' representing the names of the elemental components to be combined
#' converted into a transmittable format. This argument is fully specified
#' by the \code{x} argument of the client-side \code{ds.cbind} function.
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.cbind} (or default name \code{cbind.newobj})
#' which is written to the server-side. The output object is of class data.frame.
#' @author Paul Burton and Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
cbindDS <- function(x.names.transmit=NULL){
  
  x.names.input <- x.names.transmit
  x.names.act1 <- unlist(strsplit(x.names.input, split=","))
  x.names.act2 <- paste(x.names.act1, collapse=",")
  
  eval.code.x.names <- paste0("data.frame(", x.names.act2, ", check.names=FALSE)")
  
  output.cbind <- eval(parse(text=eval.code.x.names), envir = parent.frame())  
  colnames.act1 <- colnames(output.cbind)
  
  # Detect which columns (if any) have the '$' in their name and detach 
  # the '$' sign and any characters before that 
  detect.idx <- grep('[$]', colnames.act1)
  if(length(detect.idx) > 0){
    detach.names <- strsplit(colnames.act1[detect.idx], "\\$", perl=TRUE)
    for(i in 1:length(detach.names)){
      detach.names[i] <- detach.names[[i]][2]
    }
    colnames.act1[detect.idx] <- detach.names
  }
  
  # Check if any column names are duplicated and add a suffix ".k" to the kth replicate
  colnames.act1 <- make.names(colnames.act1, unique=TRUE)
  
  # Rename the columns of the output data.frame based on the above corrections if any.
  colnames(output.cbind) <- colnames.act1
  
  return(output.cbind)

}
# ASSIGN FUNCTION
# cbindDS
