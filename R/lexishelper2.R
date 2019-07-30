#'
#' @title Computes time periods of each subject
#' @description This is an internal function required by the client.
#' function \code{ds.lexis} to get the time periods for each subject.
#' @details If the exit time information is missing one single break
#' is returned with the a missing value.
#' @param dt the dataset that contains the original data.
#' @param ids a string character, the name of column that holds the individuals IDs.
#' @param exists, a string character, the name of column that holds the exit times.
#' @param interval a numeric vector, the sought interval widths.
#' @keywords internal
#' @return a list which contains the individuals time periods.
#' @author Gaye, A.
#'
lexishelper2 <- function(dt, ids, exits, interval){
  uniqIds <- unique(dt[,ids])
  sbreaks <- vector("list", length(uniqIds))
  for(i in 1:length(uniqIds)){
    endtime <- dt[which(dt[,ids] == uniqIds[i]), exits]
    if(is.na(endtime)){
      sbreaks[[i]] <- NA
    }else{
      sbreaks[[i]] <- lexishelper1(max(dt[which(dt[,ids] == uniqIds[i]), exits], na.rm=TRUE), interval)           
    }
  }
  return(sbreaks)
}  