# table2DDS (aggregate function) called by ds.table2D

This function generates a 2-dimensional contingency table where
potentially disclosive cells (based on a set threshold) are replaced by
a missing value ('NA').

## Usage

``` r
table2DDS(xvect, yvect)
```

## Arguments

- xvect:

  a numerical vector with discrete values - usually a factor.

- yvect:

  a numerical vector with discrete values - usually a factor.

## Value

a list which contains two elements: 'table', the 2-dimensional table and
'message' a message which informs about the validity of the table.

## Details

It generates 2-dimensional contingency tables where valid
(non-disclosive) tables are defined as those where none of their cells
have counts between 1 and the set threshold "nfilter.tab". When the
output table is invalid all cells except the total counts are replaced
by missing values. Only the total counts are visible on the table
returned to the client side. A message is also returned with the
2-dimensional table; the message says "invalid table - invalid counts
present" if the table is invalid and 'valid table' otherwise.

## Author

Amadou Gaye, Paul Burton, Demetris Avraam for DataSHIELD Development
Team
