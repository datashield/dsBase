# Arrange vector to pass it to the boxplot function

Arrange vector to pass it to the boxplot function

## Usage

``` r
boxPlotGG_data_Treatment_numericDS(vector.name)
```

## Arguments

- vector.name:

  `character` Name of a server-side numeric vector to arrange to be
  plotted later

## Value

`data frame` with the following structure:  

Column 'x': Names on the X axis of the boxplot, aka name of the vector
(vector.name argument)  
Column 'value': Values for that variable  

## Author

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
