#' @title Create the identity stats and necessary data to draw a plot on the client
#' 
#' @description In order to create a non disclosive box plot, the data that is passed to the client
#' is purely geometrical aspects of the plot, as a ggplot object contains all the data inside, only the graphical
#' parameters are passed. There are three different cases depending if there are grouping variables. 
#' The outliers are also removed from the graphical parameters.
#'
#' @param data_table \code{data frame} Table that holds the information to be plotted, arranged as: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka variables to plot \cr
#'  Column 'value': Values for that variable (raw data of columns rbinded) \cr
#'  Column 'group': (Optional) Values of the grouping variable \cr
#'  Column 'group2': (Optional) Values of the second grouping variable \cr
#' 
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable. 
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable. 
#'
#' @return \code{list} with: \cr
#' -\code{data frame} Geometrical parameters (identity stats of ggplot) \cr
#' -\code{character} Type of plot (single_group, double_group or no_group) \cr
#' 
#' @export

boxPlotGGDS <- function(data_table, group = NULL, group2 = NULL){
  
  ###################################################################
  # MODULE 1: CAPTURE THE subset filter SETTINGS                    #
  thr <- dsBase::listDisclosureSettingsDS()                         #
  nfilter.subset <- as.numeric(thr$nfilter.subset)                  #
  ###################################################################
  
  if(!is.null(group) & is.null(group2)){
    stats_full <- stats::aggregate(.~group+x, data_table, function(x){stats::quantile(x,c(0.05,0.25,0.5,0.75,0.95))})
    stats_n <- stats::aggregate(.~group+x, data_table, function(x){length(x)})$value
    stats <- data.frame(stats_full$value)
    stats_full$value <- NULL
    stats_full <- cbind(stats_full, stats, stats_n)
    colnames(stats_full) <- c("group", "x", "ymin", "lower", "middle", "upper", "ymax", "n")
    
    results <- list(data = stats_full, "single_group")
  }
  else if(!is.null(group) & !is.null(group2)){
    stats_full <- stats::aggregate(.~group+group2+x, data_table, function(x){stats::quantile(x,c(0.05,0.25,0.5,0.75,0.95))})
    stats_n <- stats::aggregate(.~group+group2+x, data_table, function(x){length(x)})$value
    stats <- data.frame(stats_full$value)
    stats_full$value <- NULL
    stats_full <- cbind(stats_full, stats, stats_n)
    colnames(stats_full) <- c("group", "group2", "x", "ymin", "lower", "middle", "upper", "ymax", "n")
    
    results <- list(data = stats_full, "double_group")
  }
  else{
    if("data.frame" %in% class(data_table)){
      stats_full <- stats::aggregate(.~x, data_table, function(x){stats::quantile(x,c(0.05,0.25,0.5,0.75,0.95))})
      stats_n <- stats::aggregate(.~x, data_table, function(x){length(x)})$value
      stats <- data.frame(stats_full$value)
      stats_full$value <- NULL
      stats_full <- cbind(stats_full, stats, stats_n)
      colnames(stats_full) <- c("x", "ymin", "lower", "middle", "upper", "ymax", "n")
    }
    else{
      stats_full <- stats::quantile(data_table,c(0.05,0.25,0.5,0.75,0.95))
      stats_n <- length(data_table)
      stats_full <- data.frame(t(stats_full))
      stats_full <- cbind(names(data_table)[1], stats_full, stats_n)
      colnames(stats_full) <- c("x", "ymin", "lower", "middle", "upper", "ymax", "n")
    }
    results <- list(data = stats_full, "no_group")
  }
  if(any(results$data$n < nfilter.subset)){
    stop("The selected combination of table / table + grouping yielded a subset with lower count than the allowed threshold [", nfilter.subset, "]")
  } else {
    return(results)
  }
}
