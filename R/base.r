#' Arguments to vector.
#'
#' @param ... objects to be concatenated.
#' @export
#' 
c <- function (...) {
  base::c(...)
}

#' Summary with length constraint.
#'
#' @param data vector of data
#' @export
#' 
summary <- function(data) {
  if(is.atomic(data)) {
    if(length(data) <= 1) {
      "Vector too small."
    } else {
      base::summary(data);
    }
  } else if(is.recursive(data)) {
    base::summary.default(data);
  }
}

#' Get the length of a vector.
#'
#' @param data vector of data
#' @export
#' 
length <- function (a) {
  base::length(a)
}

#' Creates a matrix from the given set of values.
#'
#' @export
#' 
matrix <- function (...) {
  base::matrix(...)
}

#' Retrieve or set the row or column names of a matrix-like object.
#'
#' @export
#' 
colnames <- function (...) {
  base::colnames(...)
}

#' Mean with NA values stripped before computation proceeds.
#'
#' @param a R object
#' @export
#' 
mean <- function (a) {
  base::mean(a,na.rm=TRUE)
}

#' Computes logarithms, by default natural logarithms.
#'
#' @param a a numeric or complex vector.
#' @export
#' 
log <- function (a) {
  base::log(a)
}

#' Computes the exponential function.
#'
#' @param a a numeric or complex vector.
#' @export
#' 
exp <- function(a) {
  base::exp(a)
}

#' Arguments to list.
#'
#' @param ...
#' @export
#' 
as.list <- function (...) {
  base::as.list(...)
}

#' Arguments to numeric.
#'
#' @param ...
#' @export
#' 
as.numeric <- function (...) {
  base::as.numeric(...)
}

#' Arguments to matrix.
#'
#' @param ...
#' @export
#' 
as.matrix <- function (...) {
  base::as.matrix(...)
}

#' Take a sequence of vector, matrix or data frames arguments and combine by columns or rows, respectively.
#' 
#' @param ... vectors or matrices.
#' @export
#' 
cbind <- function (...) {
  base::cbind(...)
}