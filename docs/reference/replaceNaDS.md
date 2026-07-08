# Replaces the missing values in a vector

This function identifies missing values and replaces them by a value or
values specified by the analyst.

## Usage

``` r
replaceNaDS(xvect, replacements)
```

## Arguments

- xvect:

  a character, the name of the vector to process.

- replacements:

  a vector which contains the replacement value(s), a vector one or more
  values for each study.

## Value

a new vector without missing values

## Details

This function is used when the analyst prefer or requires complete
vectors. It is then possible the specify one value for each missing
value by first returning the number of missing values using the function
`numNaDS` but in most cases it might be more sensible to replace all
missing values by one specific value e.g. replace all missing values in
a vector by the mean or median value. Once the missing values have been
replaced a new vector is created.

## Author

Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
