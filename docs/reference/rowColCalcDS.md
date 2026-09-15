# Computes sums and means of rows or columns of numeric arrays

The function is similar to R base functions 'rowSums', 'colSums',
'rowMeans' and 'colMeans'.

## Usage

``` r
rowColCalcDS(dataset, operation)
```

## Arguments

- dataset:

  an array of two or more dimensions.

- operation:

  an integer that indicates the operation to carry out: 1 for 'rowSums',
  2 for 'colSums', 3 for 'rowMeans' or 4 for 'colMeans'

## Value

a numeric vector

## Details

the output is returned to the user only the number of entries in the
output vector is greater or equal to the allowed size.

## Author

Gaye, A.
