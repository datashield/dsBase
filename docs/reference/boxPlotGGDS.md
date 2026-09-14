# Create the identity stats and necessary data to draw a plot on the client

In order to create a non disclosive box plot, the data that is passed to
the client is purely geometrical aspects of the plot, as a ggplot object
contains all the data inside, only the graphical parameters are passed.
There are three different cases depending if there are grouping
variables. The outliers are also removed from the graphical parameters.

## Usage

``` r
boxPlotGGDS(data_table, group = NULL, group2 = NULL)
```

## Arguments

- data_table:

  `data frame` Table that holds the information to be plotted, arranged
  as:  

  Column 'x': Names on the X axis of the boxplot, aka variables to
  plot  
  Column 'value': Values for that variable (raw data of columns
  rbinded)  
  Column 'group': (Optional) Values of the grouping variable  
  Column 'group2': (Optional) Values of the second grouping variable  

- group:

  `character` (default `NULL`) Name of the first grouping variable.

- group2:

  `character` (default `NULL`) Name of the second grouping variable.

## Value

`list` with:  
-`data frame` Geometrical parameters (identity stats of ggplot)  
-`character` Type of plot (single_group, double_group or no_group)  
