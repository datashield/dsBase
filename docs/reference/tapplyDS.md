# tapplyDS called by ds.tapply

Apply one of a selected range of functions to summarize an outcome
variable over one or more indexing factors and write the resultant
summary to the clientside

## Usage

``` r
tapplyDS(X.name, INDEX.names.transmit, FUN.name)
```

## Arguments

- X.name, :

  the name of the variable to be summarized. Specified via argument
  \<X.name\> of `ds.tapply` function

- INDEX.names.transmit, :

  the name of a single factor or a vector of names of factors to index
  the variable to be summarized. Specified via argument \<INDEX.names\>
  of `ds.tapply` function

- FUN.name, :

  the name of one of the allowable summarizing functions to be applied.
  Specified via argument \<FUN.name\> of `ds.tapply` function.

## Value

an array of the summarized values created by the tapplyDS function. This
array is returned to the clientside. It has the same number of
dimensions as INDEX.

## Details

see details for `ds.tapply` function

## Author

Paul Burton, Demetris Avraam for DataSHIELD Development Team
