#' Load a Server-Side Object by Name
#'
#' Retrieves a server-side object using `get()`. Supports both simple object
#' names (e.g. "D") and `$` column access (e.g. "D$LAB_TSC"). When `$` is
#' present, the object is retrieved first, then the named column is extracted
#' using `[[`.
#'
#' @param x A character string naming the object, optionally with "$column" syntax.
#' @return The retrieved R object, or the specified column if `$` syntax is used.
#' @noRd
.loadServersideObject <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("The input must be a single character string", call. = FALSE)
  }

  env <- parent.frame(2)

  hasColumn <- grepl("$", x, fixed = TRUE)

  if(hasColumn) {
    parts <- unlist(strsplit(x, "$", fixed = TRUE))
    obj_name <- parts[1]
    col_name <- parts[2]
  } else {
    obj_name <- x
  }

  obj <- tryCatch(
    get(obj_name, envir = env),
    error = function(e) stop("The server-side object '", x, "' does not exist")
  )

  if (hasColumn) {
    obj <- obj[[col_name]]
    if (is.null(obj)) {
      stop("Column '", col_name, "' not found in '", obj_name, "'", call. = FALSE)
    }
  }

  return(obj)
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
      "The server-side object must be of type {glue_collapse(permitted_classes, sep = ', ', last = ' or ')}. ",
      "'{obj_name}' is type {glue_collapse(typ, sep = ', ', last = ' and ')}."
    )

    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}
