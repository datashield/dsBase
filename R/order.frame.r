#' Orders a dataframme by a specific column
#'
#' @param frame a dataframe
#' @param ord the column to use for the order
#' @export
#' 

order.frame <- function (frame, ord) {
  frame[order(ord),]
}