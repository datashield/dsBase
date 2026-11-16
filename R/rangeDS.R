#'
#' @title returns the minimum and maximum of a numeric vector
#' @description this function is similar to R function \code{range} but instead to not return
#' the real minimum and maximum, the computed values are multiplied by a very small random number.
#' @param x a character string, the name of a numeric or integer vector
#' @return a numeric vector which contains the minimum and the maximum values of the vector
#' @author Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
rangeDS <- function(x) {
  xvect <- .loadServersideObject(x)
  .checkClass(obj = xvect, obj_name = x, permitted_classes = c("numeric", "integer"))

  # back-up current .Random.seed and revert on.exit
  if (exists(x = ".Random.seed", envir = globalenv())) {
      assign(x = ".old_seed", value = .Random.seed, envir = parent.frame());
      on.exit({ assign(x = ".Random.seed", value = parent.frame()$.old_seed, envir = globalenv()); remove(".old_seed", envir = parent.frame()) }, add = TRUE)
  } else
      on.exit(if (exists(x = ".Random.seed", envir = globalenv())) remove(".Random.seed", envir = globalenv()), add = TRUE)

  # print an error message if the input vector is not a numeric
  if (!(is.numeric(xvect))) {
    output <- "The input vector is not a numeric!"
  } else {
    # the study-specific seed for random number generation
    seed <- getOption("datashield.seed")
    if (is.null(seed)) {
      stop("rangeDS requires 'datashield.seed' R option to operate", call. = FALSE)
    }
    set.seed(seed)

    rr <- c(min(xvect, na.rm = TRUE), max(xvect, na.rm = TRUE))
    random1 <- stats::runif(1, 0.95, 1)
    random2 <- stats::runif(1, 1, 1.05)
    output <- c(rr[1] * random1, rr[2] * random2)
  }

  return(output)
}
# AGGREGATE FUNCTION
# rangeDS
