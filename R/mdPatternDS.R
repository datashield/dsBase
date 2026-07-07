#'
#' @title Missing data pattern with disclosure control
#' @description This function is a serverside aggregate function that computes the 
#' missing data pattern using mice::md.pattern and applies disclosure control to
#' prevent revealing small cell counts.
#' @details This function calls the mice::md.pattern function to generate a matrix
#' showing the missing data patterns in the input data. To ensure disclosure control,
#' any pattern counts that are below the threshold (nfilter.tab, default=3) are
#' suppressed.
#'
#' \strong{Suppression Method:}
#'
#' When a pattern count is below threshold:
#' - Row name is changed to "suppressed(<N>)" where N is the threshold
#' - All pattern values in that row are set to NA
#' - Summary row is also set to NA (prevents back-calculation)
#'
#' \strong{Output Matrix Structure:}
#'
#' - Rows represent different missing data patterns (plus a summary row at the bottom)
#' - Row names contain pattern counts (or "suppressed(<N>)" for invalid patterns)
#' - Columns show 1 if variable is observed, 0 if missing
#' - Last column shows total number of missing values per pattern
#' - Last row shows total number of missing values per variable
#'
#' \strong{Note for Pooling:}
#'
#' When this function is called from ds.mdPattern with type='combine', suppressed
#' patterns are excluded from pooling to prevent disclosure through subtraction.
#' This means pooled counts may underestimate the true total when patterns are
#' suppressed in some studies.
#'
#' @param x a character string specifying the name of a data frame or matrix
#' containing the data to analyze for missing patterns.
#' @return A list containing:
#' \item{pattern}{The missing data pattern matrix with disclosure control applied}
#' \item{valid}{Logical indicating if all patterns meet disclosure requirements}
#' \item{message}{A message describing the validity status}
#' @author Xavier Escribà montagut for DataSHIELD Development Team
#' @import mice
#' @export
#'
mdPatternDS <- function(x){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################

  # Parse the input data name with error handling
  x.val <- tryCatch(
    {
      eval(parse(text=x), envir = parent.frame())
    },
    error = function(e) {
      stop(paste0("Object '", x, "' does not exist on the server"), call. = FALSE)
    }
  )

  # Check object class
  typ <- class(x.val)

  # Check that input is a data frame or matrix
  if(!("data.frame" %in% typ || "matrix" %in% typ)){
    stop(paste0("The input object must be of type 'data.frame' or 'matrix'. Current type: ", 
                paste(typ, collapse = ", ")), call. = FALSE)
  }

  # Use x.val for further processing
  x <- x.val

  # Call mice::md.pattern with plot=FALSE
  pattern <- mice::md.pattern(x, plot = FALSE)

  # Apply disclosure control
  # Pattern counts are stored in row names (except last row which is empty/summary)
  # The last row contains variable-level missing counts

  validity <- "valid"
  n_patterns <- nrow(pattern) - 1  # exclude the summary row

  if(n_patterns > 0){
    # Check pattern counts (stored in row names, excluding last row)
    pattern_counts <- as.numeric(rownames(pattern)[1:n_patterns])

    # Find patterns with counts below threshold
    invalid_idx <- which(pattern_counts > 0 & pattern_counts < nfilter.tab)

    if(length(invalid_idx) > 0){
      validity <- "invalid"

      # For invalid patterns, suppress by:
      # - Setting row name to "suppressed"
      # - Setting all pattern values to NA
      rnames <- rownames(pattern)
      for(idx in invalid_idx){
        rnames[idx] <- paste0("suppressed(<", nfilter.tab, ")")
        pattern[idx, ] <- NA
      }
      rownames(pattern) <- rnames

      # Also need to recalculate the last row (summary) if patterns were suppressed
      # Set to NA to avoid disclosures
      pattern[nrow(pattern), seq_len(ncol(pattern))] <- NA
    }
  }

  # Return the pattern with validity information
  return(list(
    pattern = pattern,
    valid = (validity == "valid"),
    message = ifelse(validity == "valid", 
                     "Valid: all pattern counts meet disclosure requirements",
                     paste0("Invalid: some pattern counts below threshold (", 
                            nfilter.tab, ") have been suppressed"))
  ))
}

#AGGREGATE FUNCTION
# mdPatternDS
