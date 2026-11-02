# asListDS a serverside assign function called by ds.asList

Coerces an R object into a list

## Usage

``` r
asListDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class data.matrix. Must
  be specified in inverted commas. But this argument is usually
  specified directly by \<x.name\> argument of the clientside function
  `ds.asList`

## Value

the input object coerced to a list, which `ds.asList` writes to the
serverside. The way that `as.list` coerces objects to list depends on
the class of the object

## Details

This assign function is based on the native R function `as.list` and so
additional information can be found in the help for `as.list`

## Author

Amadou Gaye, Paul Burton for DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
