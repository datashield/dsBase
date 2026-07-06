# tapplyDS.assign called by ds.tapply.assign

Apply one of a selected range of functions to summarize an outcome
variable over one or more indexing factors and write the resultant
summary as a newobj on the serverside

## Usage

``` r
tapplyDS.assign(X.name, INDEX.names.transmit, FUN.name)
```

## Arguments

- X.name, :

  the name of the variable to be summarized. Specified via argument
  \<X.name\> of `ds.tapply.assign` function

- INDEX.names.transmit, :

  the name of a single factor or a vector of names of factors to index
  the variable to be summarized. Specified via argument \<INDEX.names\>
  of `ds.tapply.assign` function

- FUN.name, :

  the name of one of the allowable summarizing functions to be applied.
  Specified via argument \<FUN.name\> of `ds.tapply.assign` function.

## Value

an array of the summarized values created by the `tapplyDS.assign`
function. This array is written as a newobj on the serverside. It has
the same number of dimensions as INDEX.

## Details

see details for `ds.tapply.assign` function

## Author

Paul Burton, Demetris Avraam for DataSHIELD Development Team
