#'
#' @title Computes time breaks given the exit time
#' @description This is an internal function required by the client.
#' function \code{ds.lexis} to get the interval break points for each subject.
#' @details The strategy used in this function consists of walking through the 
#' specified interval width and creating interval. Each time an interval is 
#' created the exit time is reduced by the same magnitude of time. When the remaining
#' exit time is smaller than the next interval to create to process stops and 
#' that last interval is then the interval in which the subject fails. The remaining 
#' exit time is in fact the survival time in the last interval where the subject failed.
#' @param stopTime a numeric, the exit time time. 
#' @param interval a numeric vector of one or more elements which give the sought
#' interval width.
#' @keywords internal
#' @return a list which contains the break points for each subject.
#' @author Gaye, A.
#' 
lexishelper1 <- function(stopTime, interval){
  if(length(interval) < 2){
    output <- unique(c(seq(0,stopTime,by=interval),stopTime))
  }else{
    output <- c(0)
    timeleft <- stopTime
    totaltimesurvived <- 0
    count <- 1
    while(interval[count] < timeleft){
      totaltimesurvived <- totaltimesurvived + interval[count]
      if(timeleft > interval[count]){
        output <- append(output, totaltimesurvived)
      }else{
        break
      }
      timeleft <- timeleft - interval[count]
      count <- count + 1
    }
    output <- c(output, stopTime)
  }
  return(output)
}