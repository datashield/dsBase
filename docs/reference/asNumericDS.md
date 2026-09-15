# Coerces an R object into class numeric

This function is based on the native R function `as.numeric`.

## Usage

``` r
asNumericDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class numeric. Must be
  specified in inverted commas. But this argument is usually specified
  directly by \<x.name\> argument of the clientside function
  `ds.asNumeric`.

## Value

the object specified by the \<newobj\> argument (or its default name
\<x.name\>.num) which is written to the serverside. For further details
see help on the clientside function `ds.asNumeric`.

## Details

See help for function `as.numeric` in native R, and details section in
the help file of the clientside function `ds.asNumeric`.

## Author

Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development
Team
