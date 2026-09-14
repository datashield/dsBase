# matrixTransposeDS serverside assign function called by ds.matrixTranspose

Transposes a matrix A and writes the output to the serverside

## Usage

``` r
matrixTransposeDS(M1.name = NULL)
```

## Arguments

- M1.name:

  A character string specifying the name of the matrix to be transposed

## Value

Output is the matrix representing the transpose of A which is written to
the serverside. For more details see help for ds.matrixTranspose

## Details

Undertakes standard matrix transposition. This operation converts matrix
A to matrix C where element C\[i,j\] of matrix C equals element A\[j,i\]
of matrix A. Matrix A therefore has the same number of rows as matrix C
has columns and vice versa.

## Author

Paul Burton for DataSHIELD Development Team
