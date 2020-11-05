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
#' @return If there are no grouping variables: \cr
#' 
#' \code{list} with: \cr
#' -\code{data frame} Geometrical parameters (identity stats of ggplot) \cr
#' -\code{character list} Names of the variables plotted \cr
#' -\code{tbl} Counts of each variable, output of dplyr::count (used on the client for split/pooled) \cr
#' 
#' If there is one grouping variable: \cr
#' 
#' \code{list} with: \cr
#' -\code{data frame} Geometrical parameters (identity stats of ggplot) \cr
#' -\code{character list} Names of the variables plotted \cr
#' -\code{character list} Names of the grouping factors \cr
#' -\code{tbl} Counts of each variable (grouped), output of dplyr::count (used on the client for split/pooled) \cr
#' 
#' If there are two grouping variables: \cr
#' 
#' \code{list} with: \cr
#' -\code{data frame} Geometrical parameters (identity stats of ggplot) \cr
#' -\code{character list} Names of the variables plotted \cr
#' -\code{character list} Names of the first grouping factors \cr
#' -\code{character list} Names of the second grouping factors \cr
#' -\code{tbl} Counts of each variable (grouped), output of dplyr::count (used on the client for split/pooled) \cr
#' 
#' @export

boxPlotGGDS <- function(data_table, group = NULL, group2 = NULL){
  x <- NULL
  if (!is.null(group)) {
    if(!is.null(group2)) {
      plot_ret <- ggplot2::ggplot(data_table, ggplot2::aes_string(x = "x", y = "value", fill = "group")) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~group2)
      
    } else {
      plot_ret <- ggplot2::ggplot(data_table, ggplot2::aes_string(x = "x", y = "value", fill = "group")) +
      ggplot2::geom_boxplot()
      
    }
  } else {
    plot_ret <- ggplot2::ggplot(data_table, ggplot2::aes_string(x = "x", y = "value")) +
    ggplot2::geom_boxplot()
  }
  
  plot_ret <- plot_ret + ggplot2::scale_fill_brewer()
  plt <- ggplot2::ggplot_build(plot_ret)

  if(!is.null(group) & is.null(group2)){
    plt$data[[1]]$fill <- unique(data_table$group)
    return(list(data = plt$data[[1]][, !names(plt$data[[1]]) %in% c("outliers", "ymin_final", "ymax_final", 
                                                                    "notchupper", "notchlower", "xmin", "xmax",
                                                                    "weight", "colour", "size", "flipped_aes",
                                                                    "alpha", "shape", "linetype",
                                                                    "x", "xid", "newx", "new_width")], unique(data_table$x),
                unique(data_table$group),
                counts = dplyr::count(data_table, x, group),
                "single_group"))
  }
  else if(!is.null(group) & !is.null(group2)){
    return(plt)
    plt$data[[1]]$fill <- unique(data_table$group)
    return(list(data = plt$data[[1]][, !names(plt$data[[1]]) %in% c("outliers", "ymin_final", "ymax_final", 
                                                                    "notchupper", "notchlower", "xmin", "xmax",
                                                                    "weight", "colour", "size", "flipped_aes",
                                                                    "alpha", "shape", "linetype",
                                                                    "x", "xid", "newx", "new_width")], unique(data_table$x),
                unique(data_table$group), 
                unique(data_table$group2),
                counts = dplyr::count(data_table, x, group, group2),
                "double_group"))
  }
  else{
    plt$data[[1]]$fill <- unique(data_table$group)
    return(list(data = plt$data[[1]][, !names(plt$data[[1]]) %in% c("outliers", "ymin_final", "ymax_final", 
                                                                    "notchupper", "notchlower", "xmin", "xmax",
                                                                    "weight", "colour", "size", "flipped_aes",
                                                                    "alpha", "shape", "linetype",
                                                                    "x", "xid", "newx", "new_width")], unique(data_table$x),
                counts = dplyr::count(data_table, x),
                "no_group"))
  }
  
}