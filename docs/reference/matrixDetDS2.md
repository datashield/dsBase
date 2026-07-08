# matrixDetDS assign function called by ds.matrixDet

Calculates the determinant of a square matrix A and writes the output to
the serverside

## Usage

``` r
matrixDetDS2(M1.name = NULL, logarithm)
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
which is written to the serverside. For more details see help for
ds.matrixDet

## Details

Calculates the determinant of a square matrix (for additional
information see help for `det` function in native R). This operation is
only possible if the number of columns and rows of A are the same.

## Author

Paul Burton for DataSHIELD Development Team
