# Return the names of a list object

Returns the names of a designated server-side list

## Usage

``` r
namesDS(xname.transmit)
```

## Arguments

- xname.transmit:

  a character string specifying the name of the list.

## Value

`namesDS` returns to the client-side the names of a list object stored
on the server-side.

## Details

namesDS is an aggregate function called by ds.names. This function is
similar to the native R function `names` but it does not subsume all
functionality, for example, it only works to extract names that already
exist, not to create new names for objects. The function is restricted
to objects of type list, but this includes objects that have a primary
class other than list but which return TRUE to the native R function
`is.list`. As an example, this includes the multi-component object
created by fitting a generalized linear model using ds.glmSLMA. The
resultant object saved on each separate server is formally of double
class "glm" and "ls" but responds TRUE to is.list(),

## Author

Amadou Gaye, updated by Paul Burton 25/06/2020
