# Coerces an R object into class integer

This function is based on the native R function `as.integer`.

## Usage

``` r
asIntegerDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class integer. Must be
  specified in inverted commas. But this argument is usually specified
  directly by \<x.name\> argument of the clientside function
  `ds.asInteger`.

## Value

the object specified by the \<newobj\> argument (or its default name
"asinteger.newobj") which is written to the serverside. For further
details see help on the clientside function `ds.asInteger`.

## Details

See help for function `as.integer` in native R, and details section in
the help file of the clientside function `ds.asInteger`.

## Author

Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development
Team
