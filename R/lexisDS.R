#' 
#' @title Generates an expanded version of a dataset that contains survival data
#' @description This function is meant to be used as part of a piecewise regression analysis.
#' @details It splits the survial interval time of subjects into sub-intervals and reports the failure 
#' status of the subjects at each sub-interval. Each of those sub-interval is given an id e.g. if the overall
#' interval of a subject is split into 4 sub-interval, those sub-intervals have ids 1, 2, 3 and 4; so this is 
#' basically the count of periods for each subject. The interval ids are held in a column named "TIMEID". 
#' The entry and exit times in the input table are used to compute the total survival time. 
#' By default all the covariates in the input table are included in the expanded output table but it is 
#' preferable to indicate the names of the covariates to be included via the argument 'variables'.
#' @param data a character, the name of the data frame that holds the original data, this is the data to be expanded.
#' @param intervalWidth, a numeric vector which gives the chosen width of the intervals ('pieces'). 
#' This can be one value (in which case all the intervals that same width) or several different values.
#' If no value(s) is(are) provided a single default value is used. hat default value is the set to be the 
#' 1/10th of the mean across all the studies.
#' @param idCol a characte,r the name of the column that holds the individual IDs of the subjects.
#' @param entryCol a character, the name of the column that holds the entry times (i.e. start of follow up).
#' If no name is provided the default is to set all the entry times to 0 in a column named "STARTTIME".
#' A message is then printed to alert the user as this has serious consequences if the actual entry times are 
#' not 0 for all the subjects. 
#' @param exitCol a character, the name of the column that holds the exit times (i.e. end of follow up).
#' @param statusCol a character, the name of the column that holds the 'failure' status of each subject, 
#' tells whether or not a subject has been censored.
#' @param variables a character vector, the column names of the variables (covariates) to include in the 
#' final expanded table. The input table might have a large number of covariates and if only some of those
#' variables are relevant for the sought analysis it make sense to only include those. By default (i.e. if
#' no variables are indicated) all the covariates in the inout table are included and this will lengthen the
#' run time of the function. 
#' @return a dataframe, an expanded version of the input tabl.
#' @author Gaye, A.
#' @export
#' 
lexisDS <- function(data, intervalWidth, idCol, entryCol, exitCol, statusCol, variables){
  
  dataset <- eval(parse(text=data))
  
  # if entry time is not provided set all entry times to 0
  if(is.null(entryCol)){
    entryCol <- "STARTTIME"
    STARTTIME <- rep(0, dim(dataset)[1])
    dataset <- cbind(dataset, STARTTIME )
  }
  
  # get each subject's time period. Here we call the function 'getBreaks' 
  # which which uses the function 'personBreaks'
  sbreaks <- lexishelper2(dataset, idCol, exitCol, intervalWidth)
  missingbreaks <- which(is.na(sbreaks))
  
  # get the extended period of observation (the left time points i.e. 
  # 'entry points' and right time points i.e. 'exit points')
  starts <- lapply(sbreaks, function(x) x[-length(x)])  
  starts[missingbreaks] <- NA
  stops <-  lapply(sbreaks, function(x) x[-1])
  stops[missingbreaks] <- NA
  
  # just used to deal with. 'cumsum' (cumulative sums) get the index of the 
  # last observation for each subject.
  count.per.id <- sapply(starts, length)
  index <- tapply(dataset[,idCol], dataset[,idCol], length)
  index <- cumsum(index) 
  
  event <- rep(0,sum(count.per.id))
  event[cumsum(count.per.id)] <- dataset[index, statusCol]  
  
  # counts of period for each individual
  xx <- list()
  for(i in 1:length(count.per.id)){ xx[[i]] <- seq(1,count.per.id[i])}
  TIMEID <- unlist(xx)
  
  # use all what has been created to generate the expanded table
  myIds <- rep(dataset[index, idCol], count.per.id)
  expandedTable <- data.frame(myIds, unlist(starts), unlist(stops), event, TIMEID) 
  colnames(expandedTable) <- c(idCol, entryCol, exitCol, statusCol, "TIMEID")
  
  # create the period of observation
  SURVIVALTIME <- expandedTable[, exitCol] - expandedTable[, entryCol]
  expandedTable <- cbind(expandedTable, SURVIVALTIME)
  
  # add all other variables on the input table
  if(is.null(variables)){
    varnames <- colnames(dataset)
    vars2add <- varnames[which(!(varnames %in% colnames(expandedTable)))]    
  }else{
    vars2add <- variables
  }
  cols <- colnames(expandedTable)
  for(i in 1:length(vars2add)){
    xx <- list()
    var <- dataset[, vars2add[i]]
    for(j in 1:length(var)){ xx[[j]] <- rep(var[j], count.per.id[j])}
    assign(vars2add[i], unlist(xx))
    expandedTable <- cbind(expandedTable, eval(parse(text=vars2add[i])))  
    cols <- append(cols, vars2add[i])                       
  }
  colnames(expandedTable) <- cols
  
  return(expandedTable)
  
}