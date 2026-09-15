# matrixInvertDS serverside assign function called by ds.matrixInvert

Inverts a square matrix A and writes the output to the serverside

## Usage

``` r
matrixInvertDS(M1.name = NULL)
```

## Arguments

- M1.name:

  A character string specifying the name of the matrix to be inverted

## Value

Output is the matrix representing the inverse of A which is written to
the serverside. For more details see help for ds.matrixInvert

## Details

Undertakes standard matrix inversion. This operation is only possible if
the number of columns and rows of A are the same and the matrix is
non-singular - positive definite (eg there is no row or column that is
all zeros)

## Author

Paul Burton for DataSHIELD Development Team
