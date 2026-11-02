#'
#'  @title
#' lexis3DS
#'
#' @description
#' The third serverside function called by ds.lexis.
#' @details This is an assign function that simplifies the
#' returned output from ds.lexis. Specifically, without lexis3DS the output consists of a table within
#' a list, but lexis3DS converts this directly into a dataframe.
#' For more details see the extensive header for ds.lexis.
#' 
#' @return Data frame with `messageobj` object
#' @export
#'
lexis3DS <- function(){

  outobj <- data.frame(base::get("messageobj", envir = parent.frame())$expanded.table)

  return(outobj)

}
#ASSIGN FUNCTION
# lexis3DS
