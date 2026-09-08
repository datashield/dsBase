# matrixDetDS aggregate function called by ds.matrixDet.report

Calculates the determinant of a square matrix A and returns the output
to the clientside

## Usage

``` r
matrixDetDS1(M1.name = NULL, logarithm)
```

## Arguments

- M1.name:

  A character string specifying the name of the matrix for which
  determinant to be calculated

- logarithm:

  logical. Default is FALSE, which returns the determinant itself, TRUE
  returns the logarithm of the modulus of the determinant.

## Value

Output is the determinant of the matrix identified by argument \<M1\>
which is returned to the clientside. For more details see help for
ds.matrixDet

## Details

Calculates the determinant of a square matrix (for additional
information see help for `det` function in native R). This operation is
only possible if the number of columns and rows of A are the same.

## Author

Paul Burton for DataSHIELD Development Team
