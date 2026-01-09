#' Load a Server-Side Object by Name
#'
#' Evaluates a character string referring to an object name and returns the corresponding
#' object from the parent environment. If the object does not exist, an error is raised.
#'
#' @param x A character string naming the object to be retrieved.
#' @return The evaluated R object referred to by `x`.
#' @noRd
.loadServersideObject <- function(x) {
  tryCatch(
    eval(parse(text = x), envir = parent.frame(2)), 
    error = function(e) {
      stop("The server-side object", " '", x, "' ", "does not exist")
    }
  )
}

#' Check Class of a Server-Side Object
#'
#' Verifies that a given object is of an allowed class. If not, raises an informative error
#' message listing the permitted classes and the actual class of the object.
#'
#' @param obj The object whose class should be checked.
#' @param obj_name A character string with the name of the object (used in error messages).
#' @param permitted_classes A character vector of allowed class names.
#' @importFrom glue glue glue_collapse
#' @return Invisibly returns `TRUE` if the class check passes; otherwise throws an error.
#' @noRd
.checkClass <- function(obj, obj_name, permitted_classes) {
  typ <- class(obj)
  
  if (!any(permitted_classes %in% typ)) {
    msg <- glue(
      "The server-side object must be of type {glue_collapse(permitted_classes, sep = ' or ')}. ",
      "'{obj_name}' is type {typ}."
    )
    
    stop(msg, call. = FALSE)
  }
  
  invisible(TRUE)
}
