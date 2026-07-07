# matrixDS assign function called by ds.matrix

Creates a matrix A on the serverside

## Usage

``` r
matrixDS(mdata.transmit, from, nrows.transmit, ncols.transmit, byrow, dimnames)
```

## Arguments

- mdata.transmit:

  specifies the elements of the matrix to be created. Fully specified by
  \<mdata\> argument of ds.matrix

- from:

  a character string specifying the source and nature of \<mdata\>.
  Fully specified by \<from\> argument of ds.matrix

- nrows.transmit:

  specifies the number of rows in the matrix to be created. Fully
  specified by \<nrows.scalar\> argument of ds.matrix

- ncols.transmit:

  specifies the number of columns in the matrix to be created. Fully
  specified by \<ncols.scalar\> argument of ds.matrix

- byrow:

  a logical value specifying whether, when \<mdata\> is a vector, the
  matrix created should be filled row by row or column by column. Fully
  specified by \<byrow\> argument of ds.matrix

- dimnames:

  A dimnames attribute for the matrix: NULL or a list of length 2 giving
  the row and column names respectively. An empty list is treated as
  NULL, and a list of length one as row names only. Fully specified by
  \<dimnames\> argument of ds.matrix

## Value

Output is the matrix A written to the serverside. For more details see
help for ds.matrix

## Details

Similar to the [`matrix()`](https://rdrr.io/r/base/matrix.html) function
in native R. Creates a matrix with dimensions specified by
\<nrows.scalar\> and \<ncols.scalar\> arguments and assigns the values
of all its elements based on the \<mdata\> argument

## Author

Paul Burton for DataSHIELD Development Team
