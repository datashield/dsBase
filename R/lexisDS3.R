#'
#'  @title
#' lexisDS3
#'
#' @description
#' The third serverside function called by ds.lexis.
#' @details This is an assign function that simplifies the
#' returned output from ds.lexis. Specifically, without lexisDS3 the output consists of a table within
#' a list, but lexisDS3 converts this directly into a dataframe.
#' For more details see the extensive header for ds.lexis.
#'
#' @export
#'
lexisDS3 <- function(){

  outobj <- data.frame(base::get("messageobj")$expanded.table)

  return(outobj)

}
#ASSIGN FUNCTION
# lexisDS3
