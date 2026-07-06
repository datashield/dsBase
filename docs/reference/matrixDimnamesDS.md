# matrixDimnamesDS assign function called by ds.matrixDimnames

Adds dimnames (row names, column names or both) to a matrix on the
serverside.

## Usage

``` r
matrixDimnamesDS(M1.name = NULL, dimnames)
```

## Arguments

- M1.name:

  Specifies the name of the serverside matrix to which dimnames are to
  be added. Fully specified by \<M1\> argument of function
  `ds.matrixDimnames`. For more details see help for
  `ds.matrixDimnames`.

- dimnames:

  A dimnames attribute for the matrix: NULL or a list of length 2 giving
  the row and column names respectively. Fully specified by \<dimnames\>
  argument of function `ds.matrixDimnames`. For more details see help
  for `ds.matrixDimnames`.

## Value

Output is the serverside matrix specified by the \<newobj\> argument (or
default name diag\_\<x1\>) with specified dimnames (row and column
names) which is written to the serverside.

## Details

Adds dimnames (row names, column names or both) to a matrix on the
serverside. Similar to the `dimnames` function in native R. For more
details see help for function `ds.matrixDimnames`

## Author

Paul Burton for DataSHIELD Development Team
