# Calculates the skewness of a numeric variable

This function calculates the skewness of a numeric variable for each
study separately.

## Usage

``` r
skewnessDS1(x, method)
```

## Arguments

- x:

  a string character, the name of a numeric variable.

- method:

  an integer between 1 and 3 selecting one of the algorithms for
  computing skewness detailed in the headers of the client-side
  `ds.skewness` function.

## Value

a list including the skewness of the input numeric variable, the number
of valid observations and the study-side validity message.

## Details

The function calculates the skewness of an input variable x with three
different methods. The method is specified by the argument `method` in
the client-side `ds.skewness` function.

## Author

Demetris Avraam, for DataSHIELD Development Team
