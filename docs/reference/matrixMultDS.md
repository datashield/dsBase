# matrixMultDS serverside assign function called by ds.matrixMult

Calculates the matrix product of two matrices and writes output to
serverside

## Usage

``` r
matrixMultDS(M1.name = NULL, M2.name = NULL)
```

## Arguments

- M1.name:

  A character string specifying the name of the first matrix (M1)
  argument specified by the M1 argument in the original call to
  ds.matrixMult

- M2.name:

  A character string specifying the name of the second matrix (M2)
  argument specified by the M1 argument in the original call to
  ds.matrixMult

## Value

Output is the matrix representing the product of M1 and M2 which is
written to the serverside. For more details see help for ds.matrixMult

## Details

Undertakes standard matrix multiplication where with input matrices A
and B with dimensions A: mxn and B: nxp the output C has dimensions mxp
and each element C\[i,j\] has value equal to the dot product of row i of
A and column j of B where the dot product is obtained as
sum(A\[i,1\]\*B\[1,j\] + A\[i,2\]\*B\[2,j\] + .... +
A\[i,n\]\*B\[n,j\]). This calculation is only valid if the number of
columns of A is the same as the number of rows of B

## Author

Paul Burton for DataSHIELD Development Team
