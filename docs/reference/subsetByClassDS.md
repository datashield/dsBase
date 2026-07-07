# Breaks down a dataframe or a factor into its sub-classes

The function takes a categorical vector or dataframe as input and
generates subset(s) vectors or dataframes for each category. Subsets are
considered invalid if they hold between 1 and 4 observations.

## Usage

``` r
subsetByClassDS(data = NULL, variables = NULL)
```

## Arguments

- data:

  a string character, the name of the dataframe or the factor vector

- variables:

  a vector of string characters, the names of the the variables to
  subset on.

## Value

a list which contains the subsetted datasets

## Details

If the input data object is a dataframe it is possible to specify the
variables to subset on. If a subset is not 'valid' all its the values
are reported as missing (i.e. NA), the name of the subsets is labelled
as '\_INVALID'. If no variables are specified to subset on, the
dataframe will be subset on each of its factor variables. And if none of
the columns holds a factor variable a message is issued as output. A
message is also issued as output if the input vector is not of type
factor.

## Author

Gaye, A.
