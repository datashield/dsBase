#' @title Arrange data frame to pass it to the boxplot function
#'
#' @param table.name \code{character} Name of a server-side data frame that holds the information to be plotted later
#' @param variables \code{character vector} Name of the column(s) of the data frame to include on the boxplot
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable.
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable.
#'
#' @return \code{data frame} with the following structure: \cr
#'
#'  Column 'x': Names on the X axis of the boxplot, aka variables to plot \cr
#'  Column 'value': Values for that variable (raw data of columns rbinded) \cr
#'  Column 'group': (Optional) Values of the grouping variable \cr
#'  Column 'group2': (Optional) Values of the second grouping variable \cr
#'
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export

boxPlotGG_data_TreatmentDS <- function(table.name, variables, group = NULL, group2 = NULL){

  table <- .loadServersideObject(table.name)

  for(variable in variables){
    .checkClass(obj = table[[variable]], obj_name = variable, permitted_classes = c("numeric", "integer"))
  }

  if(is.null(group) & !is.null(group2)){
    group <- group2
    group2 <- NULL
  }

  if(is.null(group2)){
    if(is.null(group)){
      data <- table[, c(variables)]
    }
    else{
      .checkClass(obj = table[[group]], obj_name = group, permitted_classes = c("factor"))
      data <- table[, c(variables, group)]
    }

  }
  else{
    .checkClass(obj = table[[group]], obj_name = group, permitted_classes = c("factor"))
    .checkClass(obj = table[[group2]], obj_name = group2, permitted_classes = c("factor"))
    data <- table[, c(variables, group, group2)]
    # Handle case group == group2 
    if(group == group2){
      data$group2 <- data[, group]
      group2 <- "group2"
    }
  }

  data <- reshape2::melt(data, measure.vars = variables, rm.na = TRUE,
                 variable.name="x")

  if(!is.null(group)){
    names(data)[names(data) == group] <- 'group'
    data[, "group"] <- as.factor(data[, "group"])
    data <- data[stats::complete.cases(data), ]
  }
  else{
    if(length(variables) == 1){
      data <- data[stats::complete.cases(data), ]
      names(data) <- variables
    }
    else{
      data <- data[stats::complete.cases(data), ]
    }
  }
  if(!is.null(group2)){
    names(data)[names(data) == group2] <- 'group2'
    data[, "group2"] <- as.factor(data[, "group2"])
    data <- data[stats::complete.cases(data), ]
  }

  return(data)
  
}
