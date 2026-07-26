# asListDS a serverside aggregate function called by ds.asList

Coerces an R object into a list

## Usage

``` r
asListDS(x.name, newobj)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class data.matrix. Must
  be specified in inverted commas. But this argument is usually
  specified directly by \<x.name\> argument of the clientside function
  `ds.asList`

- newobj:

  is the object hard assigned '\<\<-' to be the output of the function
  written to the serverside

## Value

the object specified by the \<newobj\> argument (or its default name
\<x.name\>.mat) which is written to the serverside. In addition, two
validity messages are returned. The first confirms an output object has
been created, the second states its class. The way that `as.list`
coerces objects to list depends on the class of the object, but in
general the class of the output object should usually be 'list'

## Details

Unlike most other class coercing functions this is an aggregate function
rather than an assign function. This is because the `datashield.assign`
function in the data repository deals specially with a created object
(newobj) if it is of class list. Reconfiguring the function as an
aggregate function works around this problem. This aggregate function is
based on the native R function `as.list` and so additional information
can be found in the help for `as.list`

## Author

Amadou Gaye, Paul Burton for DataSHIELD Development Team
