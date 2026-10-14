# Arrange data frame to pass it to the boxplot function

Arrange data frame to pass it to the boxplot function

## Usage

``` r
boxPlotGG_data_TreatmentDS(table.name, variables, group = NULL, group2 = NULL)
```

## Arguments

- table.name:

  `character` Name of a server-side data frame that holds the
  information to be plotted later

- variables:

  `character vector` Name of the column(s) of the data frame to include
  on the boxplot

- group:

  `character` (default `NULL`) Name of the first grouping variable.

- group2:

  `character` (default `NULL`) Name of the second grouping variable.

## Value

`data frame` with the following structure:  

Column 'x': Names on the X axis of the boxplot, aka variables to plot  
Column 'value': Values for that variable (raw data of columns rbinded)  
Column 'group': (Optional) Values of the grouping variable  
Column 'group2': (Optional) Values of the second grouping variable  

## Author

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
