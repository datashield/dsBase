# matrixDiagDS assign function called by ds.matrixDiag

Extracts the diagonal vector from a square matrix A or creates a
diagonal matrix A based on a vector or a scalar value and writes the
output to the serverside

## Usage

``` r
matrixDiagDS(x1.transmit, aim, nrows.transmit)
```

## Arguments

- x1.transmit:

  identifies the input matrix or vector. Fully specified by \<x1\>
  argument of `ds.matrixDiag`. For more details see help for
  `ds.matrixDiag`.

- aim:

  a character string specifying what behaviour is required of the
  function. Fully specified by \<aim\> argument of `ds.matrixDiag`. For
  more details see help for `ds.matrixDiag`.

- nrows.transmit:

  a scalar value forcing the number of rows and columns in an output
  matrix.Fully specified by \<nrows.scalar\> argument of
  `ds.matrixDiag`. For more details see help for `ds.matrixDiag`.

## Value

Output is the matrix or vector specified by the \<newobj\> argument (or
default name diag\_\<x1\>) which is written to the serverside. For more
details see help for `ds.matrixDiag`.

## Details

For details see help for function `ds.matrixDiag`.

## Author

Paul Burton for DataSHIELD Development Team
